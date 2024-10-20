package de.invesdwin.context.haskell.runtime.ghci.pool;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.lang3.mutable.MutableInt;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;

import de.invesdwin.context.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.context.haskell.runtime.ghci.GhciProperties;
import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.time.Instant;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class ModifiedGhciBridge {

    public static final String STARTUP_SCRIPT = "import Data.Aeson";
    private static final String PROMPT = "ghci> ";
    private static final byte[] PROMPT_BYTES = PROMPT.getBytes();
    private static final char NEW_LINE = '\n';
    private static final String TERMINATOR_RAW = "__##@@##__";
    private static final String TERMINATOR = "\"" + TERMINATOR_RAW + "\"";
    private static final String TERMINATOR_SUFFIX = "\nputStrLn " + TERMINATOR;
    private static final byte[] TERMINATOR_SUFFIX_BYTES = TERMINATOR_SUFFIX.getBytes();

    private final ProcessBuilder jbuilder;
    private Process ghci = null;
    private InputStream inp = null;
    private ModifiedGhciErrorConsoleWatcher errWatcher = null;
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
    private final byte[] promptBuf = new byte[PROMPT.length()];

    private final List<String> rsp = new ArrayList<>();

    ////// public API

    /**
     * Creates a Java-Ghci bridge with default settings.
     */
    public ModifiedGhciBridge() {
        jbuilder = new ProcessBuilder(Strings.splitPreserveAllTokens(GhciProperties.GHCI_COMMAND, " "));
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    //CHECKSTYLE:OFF
    @Override
    public void finalize() {
        //CHECKSTYLE:ON
        close();
    }

    /**
     * Checks if Ghci process is already running.
     */
    public boolean isOpen() {
        return ghci != null;
    }

    public ModifiedGhciErrorConsoleWatcher getErrWatcher() {
        return errWatcher;
    }

    /**
     * Starts the Ghci process.
     *
     * @param timeout
     *            timeout in milliseconds for process to start.
     */
    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        ghci = jbuilder.start();
        inp = ghci.getInputStream();
        errWatcher = new ModifiedGhciErrorConsoleWatcher(ghci);
        errWatcher.startWatching();
        out = ghci.getOutputStream();
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
                    throw new IOException("Bad Ghci process");
                }
            }
            if (s.startsWith("GHCi, version ")) {
                ver = s;
                out.write(STARTUP_SCRIPT.getBytes());
                out.write(TERMINATOR_SUFFIX_BYTES);
                out.write(NEW_LINE);
                out.flush();
            } else if (s.equals(TERMINATOR_RAW)) {
                break;
            }
        }
    }

    /**
     * Stops a running Ghci process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        ghci.destroy();
        ghci = null;
        Closeables.closeQuietly(inp);
        inp = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
        ver = null;
    }

    /**
     * Gets Ghci version.
     */
    public String getGhciVersion() {
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
                if (Strings.containsAny(s, TERMINATOR_RAW, TERMINATOR)) {
                    return;
                }
                if (Strings.startsWith(s, PROMPT)) {
                    continue;
                }
                rsp.add(s);
            }
        } catch (final IOException ex) {
            throw new RuntimeException("GhciBridge connection broken", ex);
        }
    }

    public JsonNode getAsJsonNode(final String variable) {
        final StringBuilder message = new StringBuilder("__ans__ = show ( encode ( ");
        message.append(variable);
        message.append(" ) )\nputStrLn ( show ( length ( __ans__ ) ) )");
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
            throw new RuntimeException("Invalid response from Ghci REPL");
        }
        try {
            //WORKAROUND: always extract the last output as the type because the executed code might have printed another line
            String lengthStr = rsp.get(rsp.size() - 1);
            if (lengthStr.contains(" ")) {
                //sometimes parts of the prompt are included in a line (ghci is inconsistent in its printing of newlines sometimes)
                lengthStr = Strings.substringAfterLast(lengthStr, " ");
            }
            final int n = Integer.parseInt(lengthStr);
            if (n == 0) {
                //Missing or Nothing
                return null;
            }
            write("putStrLn __ans__");

            read(promptBuf);
            if (!ByteBuffers.equals(PROMPT_BYTES, promptBuf)) {
                throw new IllegalStateException(
                        "Expected default prompt \"" + PROMPT + "\" but got \"" + new String(promptBuf) + "\"");
            }

            final byte[] buf = new byte[n];
            read(buf);
            return unescape(new String(buf));
        } catch (final IOException ex) {
            throw new RuntimeException("GhciBridge connection broken", ex);
        }
    }

    private String unescape(final String str) {
        String unescaped = str;
        unescaped = Strings.removeStart(unescaped, "\"");
        unescaped = Strings.removeEnd(unescaped, "\"");
        return Strings.unescapeJava(unescaped);
    }

    /**
     * Evaluates an expression in Ghci.
     *
     * @param jcode
     *            expression to evaluate.
     * @return value of the expression.
     */
    public void eval(final String jcode) {
        exec(jcode, "> exec %s", jcode);
        checkError();
    }

    ////// private stuff

    private void write(final String s) throws IOException {
        IScriptTaskRunnerHaskell.LOG.trace("> " + s);
        out.write(s.getBytes());
        out.write(NEW_LINE);
        out.flush();
    }

    private void flush() throws IOException {
        while (inp.available() > 0) {
            inp.read();
        }
    }

    private int read(final byte[] buf) throws IOException {
        final MutableInt ofs = new MutableInt(0);
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {
            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (interruptedCheck.check()) {
                    checkError();
                }
                int n = inp.available();
                while (n > 0 && !Thread.interrupted()) {
                    final int m = buf.length - ofs.intValue();
                    ofs.add(inp.read(buf, ofs.intValue(), n > m ? m : n));
                    if (ofs.intValue() == buf.length) {
                        return true;
                    }
                    n = inp.available();
                }
                return false;
            }
        };
        try {
            spinWait.awaitFulfill(System.nanoTime());
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        IScriptTaskRunnerHaskell.LOG.trace("< (" + ofs + " bytes)");
        return ofs.intValue();
    }

    private String readline() throws IOException {
        readLineBufferPosition = 0;
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {
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
        final String replaced = Strings.replace(s, PROMPT, "");
        if (!Strings.containsAny(replaced, TERMINATOR_RAW, TERMINATOR)) {
            IScriptTaskRunnerHaskell.LOG.debug("< %s", replaced);
        }
        return replaced;
    }

    protected void checkError() {
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

}
