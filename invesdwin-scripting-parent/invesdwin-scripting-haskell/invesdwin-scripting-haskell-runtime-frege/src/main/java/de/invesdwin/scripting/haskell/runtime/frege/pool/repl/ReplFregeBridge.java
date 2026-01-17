package de.invesdwin.scripting.haskell.runtime.frege.pool.repl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.context.system.properties.SystemProperties;
import de.invesdwin.instrument.DynamicInstrumentationReflections;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.scripting.haskell.runtime.frege.pool.IFregeBridge;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.time.Instant;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;

@NotThreadSafe
public class ReplFregeBridge implements IFregeBridge {

    public static final String VALUE_START = "__##@VALUE@##__[";
    public static final String VALUE_END = "]__##@VALUE@##__";
    public static final String LENGTH_PREFIX = "__##@LENGTH@##__=";

    private static final String[] LOAD_SEARCH = new String[] { ":{", ":}" };
    private static final String[] LOAD_REPLACE = new String[] { "", "" };

    private static final File DIRECTORY = new File(ContextProperties.TEMP_DIRECTORY,
            ReplFregeBridge.class.getSimpleName());
    private static final String PROMPT = "frege> ";
    private static final char NEW_LINE = '\n';
    private static final String TERMINATOR_RAW = "__##@@##__";
    private static final String TERMINATOR = "\"" + TERMINATOR_RAW + "\"";
    private static final String TERMINATOR_SUFFIX = "\nputStrLn " + TERMINATOR;
    private static final byte[] TERMINATOR_SUFFIX_BYTES = TERMINATOR_SUFFIX.getBytes();

    private final ProcessBuilder jbuilder;
    private Process frege = null;
    private InputStream inp = null;
    private ReplFregeErrorConsoleWatcher errWatcher = null;
    private OutputStream out = null;
    private String ver = null;
    private final LoopInterruptedCheck interruptedCheck = new LoopInterruptedCheck() {
        @Override
        protected boolean onInterval() throws InterruptedException {
            //don't throw on interrupt because this makes tests flaky
            return true;
        }
    };
    private final IByteBuffer readLineBuffer = ByteBuffers.allocateExpandable();
    private int readLineBufferPosition = 0;
    private final ObjectMapper mapper;

    private final List<String> rsp = new ArrayList<>();

    ////// public API

    /**
     * Creates a Java-Frege bridge with default settings.
     */
    public ReplFregeBridge() {
        final List<String> j = new ArrayList<String>();

        final String javaExecutable = new SystemProperties().getString("java.home") + File.separator + "bin"
                + File.separator + "java";
        final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        StringBuilder classpath = new StringBuilder();
        for (final URL url : DynamicInstrumentationReflections.getURLs(classLoader)) {
            classpath.append(url.toString());
            classpath.append(File.pathSeparator);
        }
        classpath = Strings.removeEnd(classpath, ":");
        j.add(javaExecutable);
        j.add("-classpath");
        j.add(classpath.toString());
        j.add(frege.repl.FregeRepl.class.getName());
        jbuilder = new ProcessBuilder(j);
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    //CHECKSTYLE:OFF
    @Override
    public void finalize() {
        //CHECKSTYLE:ON
        close();
    }

    /**
     * Checks if Frege process is already running.
     */
    public boolean isOpen() {
        return frege != null;
    }

    public ReplFregeErrorConsoleWatcher getErrWatcher() {
        return errWatcher;
    }

    /**
     * Starts the Frege process.
     *
     * @param timeout
     *            timeout in milliseconds for process to start.
     */
    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        frege = jbuilder.start();
        inp = frege.getInputStream();
        errWatcher = new ReplFregeErrorConsoleWatcher(frege);
        errWatcher.startWatching();
        out = frege.getOutputStream();

        final Instant start = new Instant();
        while (true) {
            final String s = readline();
            if (s == null) {
                if (start.isLessThan(Duration.TEN_SECONDS)) {
                    try {
                        FTimeUnit.MILLISECONDS.sleep(1);
                    } catch (final InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                    continue;
                } else {
                    close();
                    throw new IOException("Bad Frege process");
                }
            }
            if (s.startsWith("Welcome to Frege")) {
                ver = s;

                out.write((":l " + getStartupScript().getAbsolutePath()).getBytes());
                out.write(TERMINATOR_SUFFIX_BYTES);
                out.write(NEW_LINE);
                out.flush();
            } else if (s.equals(TERMINATOR_RAW)) {
                break;
            }
        }
    }

    protected static File getStartupScript() {
        final ClassPathResource resource = new ClassPathResource(ReplFregeBridge.class.getSimpleName() + ".fr",
                ReplFregeBridge.class);
        final File file;
        try (InputStream in = resource.getInputStream()) {
            final String script = IOUtils.toString(in, Charset.defaultCharset());

            file = new File(DIRECTORY, resource.getFilename());
            Files.writeStringToFileIfDifferent(file, script);
            return file;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Stops a running Frege process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        frege.destroy();
        frege = null;
        Closeables.closeQuietly(inp);
        inp = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
        ver = null;
    }

    /**
     * Gets Frege version.
     */
    public String getFregeVersion() {
        return ver;
    }

    private void exec(final String jcode, final String logMessage, final Object... logArgs) {
        rsp.clear();
        try {
            flush();
            IScriptTaskRunnerHaskell.LOG.debug(logMessage, logArgs);
            out.write(jcode.getBytes());
            out.write(TERMINATOR_SUFFIX_BYTES);
            out.write(NEW_LINE);
            out.flush();
            while (true) {
                final String s = readline();
                if (s == null) {
                    //retry, we were a bit too fast as it seems
                    continue;
                }
                if (Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
                    return;
                }
                if (Strings.startsWith(s, PROMPT)) {
                    continue;
                }
                if (Strings.equals(s, "()")) {
                    continue;
                }
                rsp.add(s);
            }
        } catch (final IOException ex) {
            throw new RuntimeException("FregeBridge connection broken", ex);
        }
    }

    @Override
    public JsonNode getAsJsonNode(final String variable) {
        final StringBuilder message = new StringBuilder();
        message.append("__get__ ( ");
        message.append(variable);
        message.append(" )");

        exec(message.toString(), "> get %s", variable);

        final String result = get();
        try {
            final JsonNode node = mapper.readTree(result);
            checkError();
            if (result == null) {
                checkErrorDelayed();
            }
            if (node instanceof TextNode) {
                //Frege does not support Nothing/null, we emulate it with an empty string
                final TextNode cNode = (TextNode) node;
                if (Strings.isBlankOrNullText(cNode.asText())) {
                    return null;
                }
            }
            if (node instanceof NullNode) {
                return null;
            } else {
                return node;
            }
        } catch (final Throwable t) {
            checkErrorDelayed();
            throw Throwables.propagate(t);
        }
    }

    private String get() {
        if (rsp.size() < 1) {
            throw new RuntimeException("Invalid response from Frege REPL");
        }
        //WORKAROUND: always extract the last output as the type because the executed code might have printed another line
        final int n = getRspLength();
        if (n == 0) {
            //Missing or Nothing
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        boolean append = false;
        for (int i = 0; i < rsp.size(); i++) {
            final String line = rsp.get(i);
            if (line.equals(VALUE_START)) {
                append = true;
                continue;
            }
            if (line.equals(VALUE_END)) {
                break;
            }
            if (append) {
                if (sb.length() > 0) {
                    sb.append("\n");
                }
                sb.append(line);
            }
        }
        if (sb.length() != n) {
            throw new IllegalStateException(
                    "resultLength[" + sb.length() + "] != expectedLength[" + n + "]: " + sb.toString());
        }
        return sb.toString();
    }

    private int getRspLength() {
        for (int i = rsp.size() - 1; i >= 0; i--) {
            final String line = rsp.get(i);
            if (line.startsWith(LENGTH_PREFIX)) {
                return Integer.parseInt(Strings.removeStart(line, LENGTH_PREFIX.length()));
            }
        }
        throw new IllegalStateException(Strings.join(rsp, "\n"));
    }

    /**
     * Evaluates an expression in Frege.
     *
     * @param jcode
     *            expression to evaluate.
     * @return value of the expression.
     */
    @Override
    public void eval(final String jcode) {
        exec(jcode, "> exec %s", jcode);
        checkError();
    }

    @Override
    public void load(final String filename, final String content) {
        final File file = new File(DIRECTORY, filename);
        Files.writeStringToFileIfDifferent(file, Strings.replaceEach(content, LOAD_SEARCH, LOAD_REPLACE));
        eval(":l " + file.getAbsolutePath());
        try {
            Files.delete(file);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    ////// private stuff

    private void flush() throws IOException {
        while (inp.available() > 0) {
            inp.read();
        }
    }

    private String readline() throws IOException {
        readLineBufferPosition = 0;
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {

            @Override
            protected boolean determineSpinAllowed() {
                return false;
            }

            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (interruptedCheck.check()) {
                    checkError();
                }
                while (inp.available() > 0 && !Thread.interrupted()) {
                    final int b = inp.read();
                    if (b == NEW_LINE) {
                        return true;
                    }
                    readLineBuffer.putByte(readLineBufferPosition++, (byte) b);
                }
                return false;
            }
        };
        try {
            spinWait.awaitFulfill(System.nanoTime());
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        if (readLineBufferPosition == 0) {
            return null;
        }
        final String s = readLineBuffer.getStringUtf8(0, readLineBufferPosition);
        if (!Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
            IScriptTaskRunnerHaskell.LOG.debug("< %s", s);
        }
        return s;
    }

    protected void checkError() {
        for (int i = 0; i < rsp.size(); i++) {
            final String line = rsp.get(i);
            if (line.startsWith("E ")) {
                throw new IllegalStateException(Strings.join(rsp, "\n"));
            } else if (line.startsWith(LENGTH_PREFIX)) {
                break;
            }
        }

        final String error = getErrWatcher().getErrorMessage();
        if (error != null) {
            throw new IllegalStateException(error);
        }
    }

    private void checkErrorDelayed() {
        //give a bit of time to read the actual error
        try {
            FTimeUnit.MILLISECONDS.sleep(10);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        checkError();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
        eval(":reset");
        eval(":l " + getStartupScript().getAbsolutePath());
        getErrWatcher().clearLog();
    }

}
