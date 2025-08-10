package de.invesdwin.scripting.rust.runtime.irust.pool;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.rust.runtime.contract.IScriptTaskRunnerRust;
import de.invesdwin.scripting.rust.runtime.irust.IrustProperties;
import de.invesdwin.util.collections.Arrays;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.UUIDs;
import de.invesdwin.util.lang.string.Charsets;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class ModifiedIrustBridge {

    public static final String ADD_JSON = ":add serde_json";
    private static final byte[] ADD_JSON_BYTES = ADD_JSON.getBytes();
    private static final char NEW_LINE = '\n';
    private static final String NEW_LINE_STR = String.valueOf(NEW_LINE);

    private static final String IRUST_INPUT_START = "IRUST_INPUT_START";
    private static final byte[] IRUST_INPUT_START_BYTES = IRUST_INPUT_START.getBytes();
    private static final String IRUST_INPUT_END = "IRUST_INPUT_END";
    private static final byte[] IRUST_INPUT_END_BYTES = IRUST_INPUT_END.getBytes();

    private static final String IRUST_OUTPUT_START = "IRUST_OUTPUT_START";
    private static final byte[] IRUST_OUTPUT_START_BYTES = IRUST_OUTPUT_START.getBytes();
    private static final String IRUST_OUTPUT_END = "IRUST_OUTPUT_END";
    private static final byte[] IRUST_OUTPUT_END_BYTES = IRUST_OUTPUT_END.getBytes();

    private static final String POP = ":pop";
    private static final byte[] POP_BYTES = POP.getBytes();

    private static final String[] IRUST_ARGS = { "--bare-repl" /* , "--default-config" */ };

    private static final Duration CHECK_ERROR_DELAY = new Duration(10, FTimeUnit.MILLISECONDS);
    /*
     * remove color codes: https://stackoverflow.com/questions/14652538/remove-ascii-color-codes
     */
    private static final String COLOR_CODE_REGEX = "\u001B\\[[;\\d]*m";
    private static final Pattern COLOR_CODE_PATTERN = Pattern.compile(COLOR_CODE_REGEX);

    private final ProcessBuilder pbuilder;
    private Process irust = null;
    private InputStream inp = null;
    private ModifiedIrustErrorConsoleWatcher errWatcher = null;
    private OutputStream out = null;
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
    private final File responseFile;

    ////// public API

    /**
     * Creates a Java-IRust bridge with default settings.
     */
    public ModifiedIrustBridge() {
        final List<String> j = new ArrayList<String>();
        j.add(IrustProperties.IRUST_COMMAND);
        j.addAll(Arrays.asList(IRUST_ARGS));
        pbuilder = new ProcessBuilder(j);
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
        this.responseFile = new File(
                new File(ContextProperties.TEMP_DIRECTORY, ModifiedIrustBridge.class.getSimpleName()),
                UUIDs.newPseudoRandomUUID() + "_ans.txt");
    }

    //CHECKSTYLE:OFF
    @Override
    public void finalize() {
        //CHECKSTYLE:ON
        close();
    }

    /**
     * Checks if irust process is already running.
     */
    public boolean isOpen() {
        return irust != null;
    }

    public ModifiedIrustErrorConsoleWatcher getErrWatcher() {
        return errWatcher;
    }

    /**
     * Starts the irust process.
     *
     * @param timeout
     *            timeout in milliseconds for process to start.
     */
    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        irust = pbuilder.start();
        inp = irust.getInputStream();
        errWatcher = new ModifiedIrustErrorConsoleWatcher(irust);
        errWatcher.startWatching();
        out = irust.getOutputStream();
        start();
        Files.forceMkdirParent(responseFile);
    }

    void start() throws IOException {
        out.write(IRUST_INPUT_START_BYTES);
        out.write(ADD_JSON_BYTES);
        out.write(IRUST_INPUT_END_BYTES);
        out.flush();
        final String response = readResponse(true, Duration.FIVE_SECONDS);
        if (!"Ok!\n".equals(response)) {
            throw new IllegalStateException("Failed to execute [" + ADD_JSON + "]: " + response);
        }
    }

    /**
     * Stops a running irust process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        irust.destroy();
        irust = null;
        Closeables.closeQuietly(inp);
        inp = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
    }

    private String exec(final String jcode, final String logMessage, final Object... logArgs) {
        rsp.clear();
        try {
            flush();
            if (IScriptTaskRunnerRust.LOG.isDebugEnabled()) {
                IScriptTaskRunnerRust.LOG.debug(logMessage.replace("{", "\\{"), logArgs);
            }
            out.write(IRUST_INPUT_START_BYTES);
            out.write(jcode.getBytes());
            out.write(IRUST_INPUT_END_BYTES);
            out.flush();
            final String response = readResponse(true, null);
            return response;
        } catch (final IOException ex) {
            throw new RuntimeException("IrustBridge connection broken", ex);
        }
    }

    private void checkResponseErrors(final String response) {
        int errorsFound = 0;
        final String[] lines = Strings.splitPreserveAllTokens(response, NEW_LINE_STR);
        for (int i = 0; i < lines.length; i++) {
            final String s = lines[i];
            if (s.startsWith("[0m[1m[38;5;9merror[")) {
                errorsFound++;
            }
            if (errorsFound <= 1) {
                rsp.add(s);
            }
        }
        if (errorsFound > 0) {
            throw newRspError(errorsFound, null);
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
            errorMsg.append(" more irust rust errors (see debug logs) ...");
        }
        return new IllegalStateException(errorMsg.toString(), cause);
    }

    public JsonNode getAsJsonNode(final String variable) {
        /*
         * By writing everything in one line that is executed immediately, that line will not be added to the internal
         * script, instead it will be executed immediately. This will also execute anything that was written before the
         * get() call as long as everything was written with ; at the end of the statement (which tells irust to add it
         * to the internal script. For optimal performance, each script should always return only one result that
         * contains all data of interest, that way the buffered script is only called once, instead of each time for
         * each get() call.
         */
        final StringBuilder message = new StringBuilder("let __ans__ = ");
        message.append(variable);
        message.append("; std::fs::write(\"");
        message.append(responseFile.getAbsolutePath());
        message.append("\", serde_json::to_string(&__ans__).unwrap())");
        message.append("");
        final String response = exec(message.toString(), "> get %s", variable);
        if (!response.endsWith("Ok(())\n")) {
            throw new IllegalStateException("Invalid response: " + response);
        }

        try {
            final String result = Files.readFileToString(responseFile, Charsets.DEFAULT);
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

    /**
     * Evaluates an expression in irust.
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

    private void flush() throws IOException {
        while (inp.available() > 0) {
            inp.read();
        }
    }

    private String readResponse(final boolean checkError, final Duration timeout) throws IOException {
        readLineBufferPosition = 0;
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {
            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (checkError) {
                    if (interruptedCheck.check()) {
                        checkError();
                    }
                }
                while (inp.available() > 0 && !Thread.interrupted()) {
                    final int b = inp.read();
                    //CHECKSTYLE:OFF
                    //                    System.out.println((char) b);
                    //CHECKSTYLE:ON
                    readLineBuffer.putByte(readLineBufferPosition++, (byte) b);
                    if (ByteBuffers.endsWithReverse(readLineBuffer.sliceTo(readLineBufferPosition),
                            IRUST_OUTPUT_START_BYTES)) {
                        readLineBufferPosition = 0;
                    }
                    if (ByteBuffers.endsWithReverse(readLineBuffer.sliceTo(readLineBufferPosition),
                            IRUST_OUTPUT_END_BYTES)) {
                        readLineBufferPosition -= IRUST_OUTPUT_END_BYTES.length;
                        return true;
                    }
                }
                return false;
            }

        };
        try {
            if (timeout != null) {
                spinWait.awaitFulfill(System.nanoTime(), timeout);
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
        IScriptTaskRunnerRust.LOG.debug("< %s", s);
        if (checkError) {
            checkResponseErrors(s);
        }
        return s;
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
            CHECK_ERROR_DELAY.sleep();
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        checkError();
    }

}
