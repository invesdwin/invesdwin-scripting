package de.invesdwin.scripting.matlab.runtime.jascib.pool;

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
import com.pty4j.PtyProcessBuilder;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.scripting.matlab.runtime.jascib.JascibProperties;
import de.invesdwin.util.collections.Arrays;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.streams.buffer.bytes.ByteBuffers;
import de.invesdwin.util.streams.buffer.bytes.IByteBuffer;
import de.invesdwin.util.time.date.FTimeUnit;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class ModifiedScilabBridge {

    private static final char NEW_LINE = '\n';
    private static final String TERMINATOR_RAW = "__##@@##__";
    private static final String TERMINATOR = "'" + TERMINATOR_RAW + "'";
    private static final String TERMINATOR_SUFFIX = "\ndisp(" + TERMINATOR + ");";
    private static final byte[] TERMINATOR_SUFFIX_BYTES = TERMINATOR_SUFFIX.getBytes();

    private static final String[] SCILAB_ARGS = { "-nwni" };

    private static final byte[][] READLINE_BLACKLIST = {
            //27 | 
            //91 | [
            //48 | 0
            //109 | m
            { 27, '[', '0', 'm' }, //
            //45 | -
            //45 | -
            //62 | >
            //32 |
            { '-', '-', '>', ' ' }, //
            //27 | 
            //91 | [
            //49 | 1
            //109 | m
            { 27, '[', '1', 'm' }, //
            //27 | 
            //91 | [
            //50 | 2
            //109 | m
            { 27, '[', '2', 'm' }, //
            //27 | 
            //91 | [
            //52 | 4
            //104 | h
            //13 |
            { 27, '[', '4', 'h', Byte.MIN_VALUE }, //
            //27 | 
            //91 | [
            //52 | 4
            //108 | l
            //32 |
            { 27, '[', '4', 'l', 32 }, //
            //8 | 
            //27 | 
            //91 | [
            //48 | 0
            //109 | m
            //13 |
            { 8, 27, '[', '0', 'm', '\r' }, //
            //8 | 
            //27 | 
            //91 | [
            //48 | 0
            //109 | m
            { 8, 27, '[', '0', 'm' }, //
            //8 | 
            //27 | 
            //91 | [
            //50 | 2
            //109 | m
            { 8, 27, '[', '2', 'm' }, //
            //8 | 
            //27 | 
            //91 | [
            //52 | 4
            //104 | h
            //72 | H
            { 8, 27, '[', '4', 'h', Byte.MIN_VALUE }, //
    };

    private final PtyProcessBuilder pbuilder;
    private Process scilab = null;
    private InputStream inp = null;
    private ModifiedScilabErrorConsoleWatcher errWatcher = null;
    private OutputStream out = null;
    private String ver = null;
    private final LoopInterruptedCheck interruptedCheck = new LoopInterruptedCheck();
    private final IByteBuffer readLineBuffer = ByteBuffers.allocateExpandable();
    private int readLineBufferPosition = 0;
    private final ObjectMapper mapper;

    private final List<String> rsp = new ArrayList<>();

    ////// public API

    /**
     * Creates a Java-Scilab bridge with default settings.
     */
    public ModifiedScilabBridge() {
        final List<String> j = new ArrayList<String>();
        j.add(JascibProperties.SCILAB_COMMAND);
        j.addAll(Arrays.asList(SCILAB_ARGS));
        pbuilder = new PtyProcessBuilder();
        pbuilder.setCommand(j.toArray(Strings.EMPTY_ARRAY));
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
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
        scilab = pbuilder.start();
        inp = scilab.getInputStream();
        errWatcher = new ModifiedScilabErrorConsoleWatcher(scilab);
        errWatcher.startWatching();
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
            if (s.startsWith("Scilab ")) {
                ver = s;
                out.write(TERMINATOR_SUFFIX_BYTES);
                out.write(NEW_LINE);
                out.flush();
                terminatorRequested = true;
            } else if (s.contains(TERMINATOR)) {
                break;
            }
        }
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
        Closeables.closeQuietly(inp);
        inp = null;
        Closeables.closeQuietly(errWatcher);
        errWatcher = null;
        Closeables.closeQuietly(out);
        out = null;
        ver = null;
    }

    /**
     * Gets Scilab version.
     */
    public String getScilabVersion() {
        return ver;
    }

    private void exec(final String jcode, final String logMessage, final Object... logArgs) {
        rsp.clear();
        try {
            if (IScriptTaskRunnerMatlab.LOG.isDebugEnabled()) {
                IScriptTaskRunnerMatlab.LOG.debug(logMessage.replace("{", "\\{"), logArgs);
            }
            out.write(jcode.getBytes());
            out.write(TERMINATOR_SUFFIX_BYTES);
            out.write(NEW_LINE);
            out.flush();
            int errorsFound = 0;
            while (true) {
                final String s = readline();
                if (s == null) {
                    if (errorsFound > 0) {
                        //throw error
                        break;
                    } else {
                        //retry, we were a bit too fast as it seems
                        continue;
                    }
                }
                if (errorsFound == 0 && Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
                    return;
                }
                rsp.add(s);
                if (s.startsWith("Error: ")) {
                    errorsFound++;
                    //throw error immediately
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
        message.append("); disp(length(__ans__));");
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
            throw new RuntimeException("Invalid response from Scilab REPL");
        }
        try {
            // WORKAROUND: always extract the last output as the type because the executed
            // code might have printed another line
            final int n = Integer.parseInt(rsp.get(rsp.size() - 1));
            if (n == 0) {
                // Missing or Nothing
                return null;
            }
            write("disp(__ans__);");
            final byte[] buf = new byte[n];
            read(buf);
            return new String(buf);
        } catch (final IOException ex) {
            throw new RuntimeException("ScilabBridge connection broken", ex);
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
        exec(jcode, "> exec %s", jcode);
        checkError();
    }

    ////// private stuff

    private void write(final String s) throws IOException {
        // IScriptTaskRunnerMatlab.LOG.debug("> " + s);
        out.write(s.getBytes());
        out.write(NEW_LINE);
        out.flush();
    }

    private int read(final byte[] buf) throws IOException {
        final MutableInt ofs = new MutableInt(0);
        // WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {
            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (interruptedCheck.check()) {
                    checkError();
                }
                while (!Thread.interrupted()) {
                    final int b = inp.read();
                    if (b != -1) {
                        buf[ofs.intValue()] = (byte) b;
                        ofs.add(1);
                    }
                    if (ofs.intValue() == buf.length) {
                        return true;
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
        // IScriptTaskRunnerMatlab.LOG.debug("< (" + ofs + " bytes)");
        return ofs.intValue();
    }

    private String readline() throws IOException {
        readLineBufferPosition = 0;
        // WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {

            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (interruptedCheck.check()) {
                    checkError();
                }
                while (!Thread.interrupted()) {
                    final int b = inp.read();
                    if (b == NEW_LINE) {
                        return true;
                    }
                    //CHECKSTYLE:OFF
                    //                    System.out.println(b + " | " + (char) b);
                    //CHECKSTYLE:ON
                    readLineBuffer.putByte(readLineBufferPosition++, (byte) b);
                    checkReadlineBlacklist();
                }
                return false;
            }

            private void checkReadlineBlacklist() {
                for (int i = 0; i < READLINE_BLACKLIST.length; i++) {
                    final byte[] entry = READLINE_BLACKLIST[i];
                    if (readLineBufferPosition == entry.length) {
                        if (equals(entry, readLineBuffer.sliceTo(readLineBufferPosition))) {
                            //CHECKSTYLE:OFF
                            //                            System.out.println(" ************** reset " + i + " -> " + readLineBufferPosition);
                            //CHECKSTYLE:ON
                            readLineBufferPosition = 0;
                            return;
                        }
                    }
                }
            }

            private boolean equals(final byte[] digesta, final IByteBuffer digestb) {
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
                        //wildcard
                        continue;
                    }
                    if (a != digestb.getByte(i)) {
                        return false;
                    }
                }
                return true;
            }
        };
        try {
            spinWait.awaitFulfill(System.nanoTime());
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        boolean carriageReturnFound = false;
        while (readLineBufferPosition > 0 && readLineBuffer.getByte(readLineBufferPosition - 1) == '\r') {
            readLineBufferPosition--;
            carriageReturnFound = true;
        }
        if (readLineBufferPosition == 0) {
            return null;
        }
        final String s;
        if (readLineBufferPosition > 1 && readLineBuffer.getByte(0) == ' ' && carriageReturnFound) {
            // trim spaces at start and end
            s = readLineBuffer.getStringUtf8(1, readLineBufferPosition - 1);
        } else {
            s = readLineBuffer.getStringUtf8(0, readLineBufferPosition);
        }
        if (!Strings.equalsAny(s, TERMINATOR_RAW, TERMINATOR)) {
            IScriptTaskRunnerMatlab.LOG.debug("< %s", s);
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
        // give a bit of time to read the actual error
        try {
            FTimeUnit.MILLISECONDS.sleep(10);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        checkError();
    }

}
