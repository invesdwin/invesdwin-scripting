package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;
import com.pty4j.PtyProcessBuilder;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.scripting.matlab.runtime.jascib.JascibProperties;
import de.invesdwin.util.collections.Arrays;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.UUIDs;
import de.invesdwin.util.lang.string.Charsets;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.streams.closeable.Closeables;
import de.invesdwin.util.time.date.FTimeUnit;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class ModifiedScilabBridge {

    private static final byte[][] WINDOWS_SUFFIX_PATTERNS = { //
            { 27, '[', '0', 'K' }, //
            { 27, '[', '5', 'G' }, //
            { 27, '[', '?', '2', '5', Byte.MIN_VALUE }, //
    };
    private static final char CARRIAGE_RETURN = '\r';
    private static final char NEW_LINE = '\n';
    private static final String TERMINATOR_RAW = "__##@@##__";
    private static final String TERMINATOR = "'" + TERMINATOR_RAW + "'";
    private static final String TERMINATOR_SUFFIX = "\r\ndisp(" + TERMINATOR + ");";

    private static final String TERMINATOR_NUM_RAW = "__##NN##__";
    private static final String TERMINATOR_NUM = "'" + TERMINATOR_NUM_RAW + "'";
    private static final String TERMINATOR_NUM_SUFFIX = "\r\ndisp(" + TERMINATOR_NUM + ");";

    /**
     * Scilab 2024.0.0 hangs sometimes without a reason, use 2025.1.0 or higher instead
     */
    private static final String[] SCILAB_ARGS = { "-nwni" };

    private static final byte[][] READLINE_BLACKLIST = {
            // 27 | 
            // 91 | [
            // 48 | 0
            // 109 | m
            { 27, '[', '0', 'm' }, //
            // 45 | -
            // 62 | >
            // 32 |
            { '-', '>', ' ' }, //
            // 45 | -
            // 45 | -
            // 62 | >
            // 32 |
            { '-', '-', '>', ' ' }, //
            // 27 | 
            // 91 | [
            // 49 | 1
            // 109 | m
            { 27, '[', '1', 'm' }, //
            // 27 | 
            // 91 | [
            // 50 | 2
            // 109 | m
            { 27, '[', '2', 'm' }, //
            // 27 | 
            // 91 | [
            // 52 | 4
            // 104 | h
            // 13 |
            { 27, '[', '4', 'h', Byte.MIN_VALUE }, //
            // 27 | 
            // 91 | [
            // 52 | 4
            // 108 | l
            // 32 |
            { 27, '[', '4', 'l', 32 }, //
            // 8 | 
            // 27 | 
            // 91 | [
            // 48 | 0
            // 109 | m
            // 13 |
            { 8, 27, '[', '0', 'm', '\r' }, //
            // 8 | 
            // 27 | 
            // 91 | [
            // 48 | 0
            // 109 | m
            { 8, 27, '[', '0', 'm' }, //
            // 91 | [
            // 48 | 0
            // 109 | m
            { '[', '0', 'm' }, //
            // ------------- WINDOWS ------------
            // 27 | 
            // 91 | [
            // 48 | 0
            // 75 | K
            // 13 |
            { 27, '[', '0', 'K', '\r' }, //
            // 27 | 
            // 91 | [
            // 48 | 0
            // 75 | K
            { 27, '[', '0', 'K' }, //
            // 27 | 
            // 91 | [
            // 53 | 5
            // 71 | G
            { 27, '[', '5', 'G' }, //
            // 27 | 
            // 91 | [
            // 63 | ?
            // 50 | 2
            // 53 | 5
            // 104 | h
            { 27, '[', '?', '2', '5', Byte.MIN_VALUE }, //
    };

    private Process scilab = null;
    private ModifiedScilabErrorConsoleWatcher errWatcher = null;
    private ModifiedScilabOutputConsoleWatcher outWatcher = null;
    private OutputStream out = null;
    private String ver = null;
    private final LoopInterruptedCheck interruptedCheck = new LoopInterruptedCheck();
    private final IByteBuffer readLineBuffer = ByteBuffers.allocateExpandable();
    private int readLineBufferPosition = 0;
    private final ObjectMapper mapper;

    private final File responseFile;

    private final List<String> rsp = new ArrayList<>();

    ////// public API

    /**
     * Creates a Java-Scilab bridge with default settings.
     */
    public ModifiedScilabBridge() {
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
        this.responseFile = new File(
                new File(ContextProperties.TEMP_DIRECTORY, ModifiedScilabBridge.class.getSimpleName()),
                UUIDs.newPseudoRandomUUID() + "_ans.txt");
    }

    // CHECKSTYLE:OFF
    @Override
    public void finalize() {
        // CHECKSTYLE:ON
        close();
    }

    /**
     * Checks if Scilab process is already running.
     */
    public boolean isOpen() {
        return scilab != null;
    }

    public ModifiedScilabErrorConsoleWatcher getErrWatcher() {
        return errWatcher;
    }

    /**
     * Starts the Scilab process.
     *
     * @param timeout
     *            timeout in milliseconds for process to start.
     */
    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        final List<String> j = new ArrayList<String>();
        j.add(JascibProperties.SCILAB_COMMAND);
        j.addAll(Arrays.asList(SCILAB_ARGS));
        final PtyProcessBuilder pbuilder = new PtyProcessBuilder();
        // scilab crashes when trying to add env vars
        pbuilder.setConsole(true);
        pbuilder.setInitialColumns(10_000);
        pbuilder.setInitialRows(10_000);
        pbuilder.setCommand(j.toArray(Strings.EMPTY_ARRAY));
        scilab = pbuilder.start();
        // scilab hangs without a PTY
        //final ProcessBuilder pbuilder = new ProcessBuilder(j);
        //scilab = pbuilder.start();
        errWatcher = new ModifiedScilabErrorConsoleWatcher(scilab);
        errWatcher.startWatching();
        outWatcher = new ModifiedScilabOutputConsoleWatcher(scilab);
        outWatcher.startWatching();
        out = scilab.getOutputStream();
        boolean terminatorRequested = true;
        while (true) {
            final String s = readline();
            if (s == null) {
                if (terminatorRequested) {
                    continue;
                } else {
                    close();
                    throw new IOException("Bad Scilab process");
                }
            }
            if (s.contains("Scilab ")) {
                ver = s;
                final StringBuilder command = new StringBuilder();
                command.append(TERMINATOR_SUFFIX);
                command.append(CARRIAGE_RETURN);
                command.append(NEW_LINE);
                write(command.toString());
                terminatorRequested = true;
            } else if (s.contains(TERMINATOR_RAW)) {
                break;
            }
        }
        Files.forceMkdirParent(responseFile);
    }

    private void write(final String command) throws IOException {
        if (outWatcher.isWindows()) {
            //try to make it less likely for scilab to receive commands incompletely on windows
            //though it seems even with this, it does not solve the stability issues
            final String[] lines = Strings.splitPreserveAllTokens(command, "\n");
            for (int i = 0; i < lines.length; i++) {
                out.write(lines[i].getBytes());
                out.write(NEW_LINE);
                out.flush();
                FTimeUnit.MILLISECONDS.sleepNoInterrupt(i);
            }
        } else {
            //though linux is rather stable compared to windows
            out.write(command.getBytes());
            out.flush();
        }
    }

    public boolean isWindows() {
        return outWatcher.isWindows();
    }

    /**
     * Stops a running Scilab process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        scilab.destroy();
        scilab = null;
        Closeables.closeQuietly(outWatcher);
        outWatcher = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
        ver = null;
        Files.deleteQuietly(responseFile);
    }

    /**
     * Gets Scilab version.
     */
    public String getScilabVersion() {
        return ver;
    }

    private void exec(final boolean number, final String jcode, final String logMessage, final Object... logArgs) {
        rsp.clear();
        try {
            if (jcode != null) {
                flush();
                if (IScriptTaskRunnerMatlab.LOG.isDebugEnabled()) {
                    IScriptTaskRunnerMatlab.LOG.debug(logMessage.replace("{", "\\{"), logArgs);
                }
                final StringBuilder command = new StringBuilder(jcode.replace("\n", "\r\n").replace("\r\r\n", "\r\n"));
                if (number) {
                    command.append(TERMINATOR_NUM_SUFFIX);
                } else {
                    command.append(TERMINATOR_SUFFIX);
                }
                command.append(CARRIAGE_RETURN);
                command.append(NEW_LINE);
                write(command.toString());
            }
            int errorsFound = 0;
            while (true) {
                final String s = readline();
                if (s == null) {
                    if (errorsFound > 0) {
                        // throw error
                        break;
                    } else {
                        // retry, we were a bit too fast as it seems
                        continue;
                    }
                }
                if (number) {
                    if (errorsFound == 0 && Strings.equalsAny(s, TERMINATOR_NUM_RAW, TERMINATOR_NUM)) {
                        return;
                    }
                } else {
                    if (errorsFound == 0 && Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
                        return;
                    }
                }
                rsp.add(s);
                if (s.startsWith("Error: ") || s.startsWith("Fehler: ")) {
                    errorsFound++;
                    // throw error immediately
                    break;
                }
            }
            if (errorsFound > 0) {
                throw newRspError(errorsFound, null);
            }
        } catch (final IOException ex) {
            throw new RuntimeException("ScilabBridge connection broken", ex);
        }
    }

    private void flush() throws IOException {
        while (outWatcher.available() > 0) {
            outWatcher.read();
        }
        errWatcher.clearLog();
    }

    private IllegalStateException newRspError(final int errorsFound, final Throwable cause) {
        final StringBuilder errorMsg = new StringBuilder();
        for (int i = 0; i < rsp.size(); i++) {
            if (i > 0) {
                errorMsg.append("\n");
            }
            errorMsg.append(rsp.get(i));
        }
        if (errorsFound > 1) {
            errorMsg.append("\n ... ");
            errorMsg.append(errorsFound - 1);
            errorMsg.append(" more scilab rust errors (see debug logs) ...");
        }
        return new IllegalStateException(errorMsg.toString(), cause);
    }

    public JsonNode getAsJsonNode(final String variable) {
        final StringBuilder message = new StringBuilder("__ans__ = toJSON(");
        message.append(variable);
        message.append("); __ans__fd__ = mopen(\"");
        message.append(responseFile.getAbsolutePath());
        message.append("\", \"wt\"); mputstr(__ans__, __ans__fd__); mclose(__ans__fd__);");
        // using a different separator for the length here to make it more robust
        // against reading previous messages
        exec(true, message.toString(), "> get %s", variable);

        try {
            final String result = readFile();
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

    private String readFile() throws InterruptedException {
        while (true) {
            try {
                final String result = Files.readFileToString(responseFile, Charsets.DEFAULT);
                if (Strings.isBlank(result)) {
                    FTimeUnit.MILLISECONDS.sleep(1);
                    continue;
                }
                return result;
            } catch (final Throwable t) {
                FTimeUnit.MILLISECONDS.sleep(1);
            }
        }
    }

    /**
     * Evaluates an expression in Scilab.
     *
     * @param jcode
     *            expression to evaluate.
     * @return value of the expression.
     */
    public void eval(final String jcode) {
        exec(false, jcode, "> exec %s", jcode);
        checkError();
    }

    ////// private stuff

    private String readline() throws IOException {
        readLineBufferPosition = 0;
        // WORKAROUND: sleeping 10 ms between messages is way too slow
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
                while (!Thread.interrupted()) {
                    final int b = outWatcher.read();
                    if (b != -1) {
                        if (b == NEW_LINE) {
                            return true;
                        }
                        // CHECKSTYLE:OFF
                        // System.out.println(b + " | " + (char) b);
                        // CHECKSTYLE:ON
                        readLineBuffer.putByte(readLineBufferPosition++, (byte) b);
                        checkReadlineBlacklist();
                    } else {
                        FTimeUnit.MILLISECONDS.sleep(1);
                    }
                }
                return false;
            }
        };
        try {
            spinWait.awaitFulfill(System.nanoTime());
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        while (readLineBufferPosition > 0 && readLineBuffer.getByte(readLineBufferPosition - 1) == '\r') {
            readLineBufferPosition--;
        }
        if (readLineBufferPosition == 0) {
            return null;
        }
        //32 |
        //32 |
        //34 | "
        //95 | _
        //95 | _
        //35 | #
        //35 | #
        //64 | @
        //64 | @
        //35 | #
        //35 | #
        //95 | _
        //95 | _
        //34 | "
        //27 | 
        //91 | [
        //48 | 0
        //75 | K
        //13 |
        boolean found;
        do {
            found = false;
            for (int i = 0; i < WINDOWS_SUFFIX_PATTERNS.length; i++) {
                final byte[] pattern = WINDOWS_SUFFIX_PATTERNS[i];
                if (bufferEndsWithReverseWildcard(readLineBuffer.sliceTo(readLineBufferPosition), pattern)) {
                    readLineBufferPosition -= pattern.length;
                    found = true;
                }
            }
        } while (found);

        // 13 |
        // 32 |
        // 32 |
        // 34 | "
        // 95 | _
        // 95 | _
        // 35 | #
        // 35 | #
        // 64 | @
        // 64 | @
        // 35 | #
        // 35 | #
        // 95 | _
        // 95 | _
        // 34 | "
        // 13 |
        // trim spaces and quotes at start and end
        int start = 0;
        while (start < readLineBufferPosition) {
            final byte b = readLineBuffer.getByte(start);
            if (b == '"') {
                start++;
                if (readLineBuffer.getByte(readLineBufferPosition - 1) == '"') {
                    readLineBufferPosition--;
                }
            } else if (b == '\r' || b == ' ') {
                start++;
            } else {
                break;
            }
        }

        final int length = readLineBufferPosition - start;
        if (length <= 0) {
            return "";
        }
        final String s = readLineBuffer.getStringUtf8(start, length);
        if (!Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR, TERMINATOR_NUM_RAW, TERMINATOR_NUM)) {
            IScriptTaskRunnerMatlab.LOG.debug("< %s", s);
        }
        return s;
    }

    private void checkReadlineBlacklist() {
        for (int i = 0; i < READLINE_BLACKLIST.length; i++) {
            final byte[] entry = READLINE_BLACKLIST[i];
            for (int offset = 0; offset <= 3; offset++) {
                if (readLineBufferPosition - offset == entry.length) {
                    if (bufferEqualsWildcard(entry, readLineBuffer.slice(offset, readLineBufferPosition - offset))) {
                        // CHECKSTYLE:OFF
                        // System.out.println(" ************** reset " + i + " -> " +
                        // readLineBufferPosition);
                        // CHECKSTYLE:ON
                        readLineBufferPosition = 0;
                        return;
                    }
                }
            }
        }
    }

    private boolean bufferEqualsWildcard(final byte[] digesta, final IByteBuffer digestb) {
        if (digesta == null || digestb == null) {
            return false;
        }

        final int lenA = digesta.length;
        final int lenB = digestb.capacity();

        if (lenB == 0) {
            return lenA == 0;
        }

        if (lenA != lenB) {
            return false;
        }

        for (int i = 0; i < lenA; i++) {
            final byte a = digesta[i];
            if (a == Byte.MIN_VALUE) {
                // wildcard
                continue;
            }
            if (a != digestb.getByte(i)) {
                return false;
            }
        }
        return true;
    }

    public static boolean bufferEndsWithReverseWildcard(final IByteBuffer digesta, final byte[] digestb) {
        if (digesta == null || digestb == null) {
            return false;
        }

        final int lenBuffer = digesta.capacity();
        final int lenStart = digestb.length;

        if (lenStart == 0) {
            return true;
        }

        if (lenBuffer < lenStart) {
            return false;
        }

        final int offsetBuffer = lenBuffer - lenStart;
        for (int i = lenStart - 1; i >= 0; i--) {
            final byte b = digestb[i];
            if (b == Byte.MIN_VALUE) {
                // wildcard
                continue;
            }
            if (digesta.getByte(offsetBuffer + i) != b) {
                return false;
            }
        }
        return true;
    }

    protected void checkError() {
        final String error = getErrWatcher().getErrorMessage();
        if (error != null) {
            throw new IllegalStateException(error);
        }
    }

    private void checkErrorDelayed() {
        // give a bit of time to read the actual error
        try {
            FTimeUnit.MILLISECONDS.sleep(10);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        checkError();
    }

}
