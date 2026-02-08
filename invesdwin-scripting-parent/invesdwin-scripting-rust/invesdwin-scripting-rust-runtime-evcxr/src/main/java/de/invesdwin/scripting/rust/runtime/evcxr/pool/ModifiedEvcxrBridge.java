package de.invesdwin.scripting.rust.runtime.evcxr.pool;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.lang3.mutable.MutableInt;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.rust.runtime.contract.IScriptTaskRunnerRust;
import de.invesdwin.scripting.rust.runtime.evcxr.EvcxrProperties;
import de.invesdwin.util.collections.Arrays;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class ModifiedEvcxrBridge {

    public static final String DEP_JSON = ":dep serde_json";
    private static final String PROMPT = ">> ";
    private static final char NEW_LINE = '\n';
    private static final String TERMINATOR_RAW = "__##@@##__";
    private static final String TERMINATOR = "\"" + TERMINATOR_RAW + "\"";
    private static final String TERMINATOR_SUFFIX = "\nprintln!(" + TERMINATOR + ");";
    private static final byte[] TERMINATOR_SUFFIX_BYTES = TERMINATOR_SUFFIX.getBytes();

    private static final String[] EVCXR_ARGS = { /* "--disable-readline" /* , "--ide-mode" */ };

    private static final Duration CHECK_ERROR_DELAY = new Duration(10, FTimeUnit.MILLISECONDS);
    /*
     * remove color codes: https://stackoverflow.com/questions/14652538/remove-ascii-color-codes
     */
    private static final String COLOR_CODE_REGEX = "\u001B\\[[;\\d]*m";
    private static final Pattern COLOR_CODE_PATTERN = Pattern.compile(COLOR_CODE_REGEX);

    private final ProcessBuilder pbuilder;
    private Process evcxr = null;
    private InputStream inp = null;
    private ModifiedEvcxrErrorConsoleWatcher errWatcher = null;
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
    private boolean usePromptBuf = false;

    private final List<String> rsp = new ArrayList<>();
    private boolean ignoreError;

    ////// public API

    /**
     * Creates a Java-Evcxr bridge with default settings.
     */
    public ModifiedEvcxrBridge() {
        final List<String> j = new ArrayList<String>();
        j.add(EvcxrProperties.EVCXR_COMMAND);
        j.addAll(Arrays.asList(EVCXR_ARGS));
        pbuilder = new ProcessBuilder(j);
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    //CHECKSTYLE:OFF
    @Override
    public void finalize() {
        //CHECKSTYLE:ON
        close();
    }

    /**
     * Checks if evcxr process is already running.
     */
    public boolean isOpen() {
        return evcxr != null;
    }

    public ModifiedEvcxrErrorConsoleWatcher getErrWatcher() {
        return errWatcher;
    }

    /**
     * Starts the evcxr process.
     *
     * @param timeout
     *            timeout in milliseconds for process to start.
     */
    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        evcxr = pbuilder.start();
        inp = evcxr.getInputStream();
        errWatcher = new ModifiedEvcxrErrorConsoleWatcher(evcxr);
        errWatcher.startWatching();
        out = evcxr.getOutputStream();
        boolean versionRequested = false;
        while (true) {
            final String s = readline(false, false);
            if (s == null) {
                if (versionRequested) {
                    versionRequested = false;
                    continue;
                } else {
                    close();
                    throw new IOException("Bad evcxr process");
                }
            }
            if (s.startsWith(PROMPT)) {
                //needed when --disable-readline is configured
                usePromptBuf = true;
            }
            if (!versionRequested && ver == null) {
                out.write(":version".getBytes());
                out.write(NEW_LINE);
                out.write(DEP_JSON.getBytes());
                out.write(TERMINATOR_SUFFIX_BYTES);
                out.write(NEW_LINE);
                out.flush();
                versionRequested = true;
            } else if (versionRequested) {
                ver = s;
            } else if (s.contains(TERMINATOR_RAW)) {
                getErrWatcher().clearLog();
                break;
            }
        }
    }

    /**
     * Stops a running evcxr process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        evcxr.destroy();
        evcxr = null;
        Closeables.closeQuietly(inp);
        inp = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
        ver = null;
    }

    /**
     * Gets evcxr version.
     */
    public String getPythonVersion() {
        return ver;
    }

    private void exec(final String jcode, final String logMessage, final Object... logArgs) {
        rsp.clear();
        try {
            flush();
            if (IScriptTaskRunnerRust.LOG.isDebugEnabled()) {
                IScriptTaskRunnerRust.LOG.debug(logMessage.replace("{", "\\{"), logArgs);
            }
            out.write(jcode.getBytes());
            out.write(TERMINATOR_SUFFIX_BYTES);
            out.write(NEW_LINE);
            out.flush();
            int errorsFound = 0;
            while (true) {
                final String s = readline(true, errorsFound > 0);
                if (s == null) {
                    if (errorsFound > 0) {
                        //throw error
                        break;
                    } else {
                        //retry, we were a bit too fast as it seems
                        continue;
                    }
                }
                if (s.equals(PROMPT)) {
                    continue;
                }
                if (errorsFound == 0 && Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
                    return;
                }
                if (s.startsWith("[31mError:[0m ")) {
                    errorsFound++;
                }
                if (errorsFound <= 1) {
                    rsp.add(s);
                }
            }
            if (errorsFound > 0) {
                throw newRspError(errorsFound, null);
            }
        } catch (final IOException ex) {
            throw new RuntimeException("EvcxrBridge connection broken", ex);
        }
    }

    private IllegalStateException newRspError(final int errorsFound, final Throwable cause) {
        final StringBuilder errorMsg = new StringBuilder();
        for (int i = 0; i < rsp.size(); i++) {
            if (i > 0) {
                errorMsg.append("\n");
            }
            errorMsg.append(COLOR_CODE_PATTERN.matcher(rsp.get(i)).replaceAll(""));
        }
        if (errorsFound > 1) {
            errorMsg.append("\n ... ");
            errorMsg.append(errorsFound - 1);
            errorMsg.append(" more evcxr rust errors (see debug logs) ...");
        }
        return new IllegalStateException(errorMsg.toString(), cause);
    }

    public JsonNode getAsJsonNode(final String variable) {
        final StringBuilder message = new StringBuilder("let __ans__ = serde_json::to_string(&");
        message.append(variable);
        //CHECKSTYLE:OFF
        message.append(").unwrap(); println!(\"{}\", __ans__.len());");
        //CHECKSTYLE:ON
        exec(message.toString(), "> get %s", variable);

        final String result = get();
        try {
            final JsonNode node = mapper.readTree(result);
            checkError();
            if (result == null) {
                checkErrorDelayed();
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
            throw new RuntimeException("Invalid response from evcxr REPL");
        }
        try {
            //WORKAROUND: always extract the last output as the type because the executed code might have printed another line
            final int n;
            try {
                n = Integer.parseInt(rsp.get(rsp.size() - 1));
            } catch (final Throwable t) {
                throw newRspError(1, t);
            }
            if (n == 0) {
                //Missing or Nothing
                return null;
            }
            //CHECKSTYLE:OFF
            write("println!(\"{}\", __ans__);");
            //CHECKSTYLE:ON
            if (usePromptBuf) {
                read(promptBuf);
                final StringBuilder sb = new StringBuilder();
                if (!startsWithPrompt(promptBuf)) {
                    sb.append(new String(promptBuf));
                }
                final byte[] buf = new byte[n];
                read(buf);
                if (startsWithPrompt(buf)) {
                    sb.append(new String(buf, PROMPT.length(), n - PROMPT.length()));
                    read(promptBuf);
                    sb.append(new String(promptBuf));
                } else {
                    sb.append(new String(buf));
                }
                return sb.toString();
            } else {
                final byte[] buf = new byte[n];
                read(buf);
                return new String(buf);
            }
        } catch (final IOException ex) {
            throw new RuntimeException("EvcxrBridge connection broken", ex);
        }
    }

    /**
     * Evaluates an expression in irust.
     *
     * @param jcode
     *            expression to evaluate.
     * @return value of the expression.
     */
    public void eval(final String jcode) {
        final String[] lines = Strings.splitPreserveAllTokens(jcode, "\n");
        StringBuilder multiLine = null;
        for (int i = 0; i < lines.length; i++) {
            final String line = lines[i];
            final String lineTrim = line.trim();
            if (":{".equals(lineTrim)) {
                if (multiLine != null) {
                    evalCommand(multiLine.toString());
                    multiLine = null;
                }
                multiLine = new StringBuilder();
                continue;
            }
            if (multiLine != null) {
                if (":}".equals(lineTrim)) {
                    evalCommand(multiLine.toString());
                    multiLine = null;
                } else {
                    multiLine.append(line);
                    multiLine.append("\n");
                }
            } else {
                multiLine = new StringBuilder();
                multiLine.append(line);
            }
        }
        if (multiLine != null) {
            evalCommand(multiLine.toString());
            multiLine = null;
        }
    }

    private void evalCommand(final String jcode) {
        exec(jcode, "> exec %s", jcode);
        checkError();
    }

    public void cargoAdd(final String expression) {
        ignoreError = true;
        try {
            eval(expression);
        } finally {
            ignoreError = false;
        }
    }

    ////// private stuff

    private void write(final String s) throws IOException {
        IScriptTaskRunnerRust.LOG.trace("> " + s);
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
            protected boolean determineSpinAllowed() {
                return false;
            }

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
        IScriptTaskRunnerRust.LOG.trace("< (" + ofs + " bytes)");
        return ofs.intValue();
    }

    private String readline(final boolean checkError, final boolean errorFound) throws IOException {
        readLineBufferPosition = 0;
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {

            @Override
            protected boolean determineSpinAllowed() {
                return false;
            }

            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (checkError) {
                    if (interruptedCheck.check()) {
                        checkError();
                    }
                }
                while (inp.available() > 0 && !Thread.interrupted()) {
                    final int b = inp.read();
                    if (b == NEW_LINE) {
                        return true;
                    }
                    readLineBuffer.putByte(readLineBufferPosition++, (byte) b);
                    //check for prompt
                    if (isPrompt()) {
                        return true;
                    }
                }
                return false;
            }

        };
        try {
            if (errorFound) {
                spinWait.awaitFulfill(System.nanoTime(), CHECK_ERROR_DELAY);
            } else {
                spinWait.awaitFulfill(System.nanoTime());
            }
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        if (readLineBufferPosition == 0) {
            return null;
        }
        final String s = readLineBuffer.getStringUtf8(0, readLineBufferPosition);
        if (!Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
            IScriptTaskRunnerRust.LOG.debug("< %s", s);
        }
        return s;
    }

    private boolean isPrompt() {
        if (readLineBufferPosition != PROMPT.length()) {
            return false;
        }
        for (int i = 0; i < PROMPT.length(); i++) {
            if (readLineBuffer.getByte(i) != (byte) PROMPT.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    private boolean startsWithPrompt(final byte[] buf) {
        for (int i = 0; i < PROMPT.length(); i++) {
            if (buf[i] != (byte) PROMPT.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    protected void checkError() {
        final String error = getErrWatcher().getErrorMessage();
        if (error != null && !ignoreError) {
            throw new IllegalStateException(error);
        }
    }

    private void checkErrorDelayed() {
        //give a bit of time to read the actual error
        try {
            CHECK_ERROR_DELAY.sleep();
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        checkError();
    }

}
