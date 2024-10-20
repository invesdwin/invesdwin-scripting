package de.invesdwin.context.julia.runtime.juliacaller.pool;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.ConnectException;
import java.net.Socket;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.context.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.concurrent.loop.LoopInterruptedCheck;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.time.date.FTimeUnit;

/**
 * Forked from here: https://github.com/jbytecode/juliacaller/issues/1
 */
@NotThreadSafe
public class ModifiedJuliaCaller {

    protected static final char NEW_LINE = '\n';
    private final String pathToJulia;
    private final ObjectMapper objectMapper;
    private Socket socket;
    private BufferedWriter bufferedWriterForJuliaConsole, bufferedWriterForSocket;
    private InputStream readerForSocket;
    private final int port;
    private int maximumTriesToConnect = 300;
    private ModifiedJuliaErrorConsoleWatcher watcher;
    private Process process;
    private final LoopInterruptedCheck interruptedCheck = new LoopInterruptedCheck() {
        @Override
        protected boolean onInterval() throws InterruptedException {
            //don't throw on interrupt because this makes tests flaky
            return true;
        }
    };

    public ModifiedJuliaCaller(final String pathToJulia, final int port) {
        this.pathToJulia = pathToJulia;
        this.port = port;
        this.objectMapper = getObjectMapper();
    }

    protected ObjectMapper getObjectMapper() {
        return MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    public void setMaximumTriesToConnect(final int tries) {
        this.maximumTriesToConnect = tries;
    }

    public int getMaximumTriesToConnect() {
        return this.maximumTriesToConnect;
    }

    public void startServer() throws IOException {
        process = Runtime.getRuntime()
                .exec(pathToJulia + " -q --depwarn=no --compiled-modules=yes --banner=no --startup-file=no");
        final InputStream is = ModifiedJuliaCaller.class
                .getResourceAsStream(ModifiedJuliaCaller.class.getSimpleName() + ".jl");
        final BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        final StringBuilder sb = new StringBuilder();
        while (true) {
            final String line = reader.readLine();
            if (line == null) {
                break;
            }
            sb.append(line);
            sb.append("\n");
        }
        reader.close();
        is.close();
        bufferedWriterForJuliaConsole = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));

        watcher = new ModifiedJuliaErrorConsoleWatcher(process);
        watcher.startWatching();

        bufferedWriterForJuliaConsole.write(sb.toString());
        bufferedWriterForJuliaConsole.newLine();
        final boolean debug = isDebugOutputEnabled();
        IScriptTaskRunnerJulia.LOG.trace("startServer: Sending serve(%s,%s) request.", this.port, debug);
        bufferedWriterForJuliaConsole.write("serve(" + this.port + ", " + debug + ")");
        bufferedWriterForJuliaConsole.newLine();
        bufferedWriterForJuliaConsole.flush();
    }

    protected boolean isDebugOutputEnabled() {
        return IScriptTaskRunnerJulia.LOG.isDebugEnabled();
    }

    public ModifiedJuliaErrorConsoleWatcher getWatcher() {
        return watcher;
    }

    public void connect() throws IOException {
        int numtries = 1;
        boolean connected = false;
        while (numtries <= this.maximumTriesToConnect) {
            try {
                socket = new Socket("localhost", this.port);
                connected = true;
                IScriptTaskRunnerJulia.LOG.trace("Connect: connected!");
                break;
            } catch (final ConnectException ce) {
                numtries++;
                try {
                    IScriptTaskRunnerJulia.LOG.trace("Connect: retrying to connect: %s / %s", numtries,
                            maximumTriesToConnect);
                    Thread.sleep(1000);
                } catch (final InterruptedException ie) {
                    throw new RuntimeException(ie);
                }
            }
        }
        if (connected) {
            bufferedWriterForSocket = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
            readerForSocket = socket.getInputStream();
        } else {
            throw new IllegalStateException(
                    "Socket cannot connect in maximum number of iterations defined as " + maximumTriesToConnect);
        }
    }

    public synchronized void execute(final String command) throws IOException {
        IScriptTaskRunnerJulia.LOG.trace("execute: Sending '%s'", command);
        //WORKAROUND: begin/end make sure that multiple lines are executed together, also newlines need to be escaped
        //without this we get: Error: Base.Meta.ParseError("extra token after end of expression")
        final String newlineEscaped = "__##@NL@##__";
        bufferedWriterForSocket.write("execute begin "
                + Strings.normalizeNewlines(command.replace("\n", newlineEscaped) + newlineEscaped + "end"));
        bufferedWriterForSocket.newLine();
        bufferedWriterForSocket.flush();
        checkError();
    }

    public void exitSession() throws IOException {
        bufferedWriterForSocket.write("exit");
        bufferedWriterForSocket.newLine();
        bufferedWriterForSocket.flush();
    }

    public void shutdownServer() throws IOException {
        watcher.close();
        watcher = null;
        bufferedWriterForSocket.write("shutdown");
        bufferedWriterForSocket.newLine();
        bufferedWriterForSocket.flush();
    }

    public JsonNode getAsJsonNode(final String varname) throws IOException {
        IScriptTaskRunnerJulia.LOG.trace("getAsJsonNode: Requesting variable %s", varname);
        bufferedWriterForSocket.write("get " + varname);
        bufferedWriterForSocket.newLine();
        bufferedWriterForSocket.flush();
        final String result = readLine();
        checkError();
        if (result == null) {
            checkErrorDelayed();
        }
        try {
            final JsonNode node = objectMapper.readTree(result).get(varname);
            if (node instanceof NullNode) {
                return null;
            } else {
                return node;
            }
        } catch (final Throwable t) {
            checkErrorDelayed();
            throw t;
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

    private void checkError() {
        final String error = getWatcher().getErrorMessage();
        if (error != null) {
            throw new IllegalStateException(error);
        }
    }

    private String readLine() throws IOException {
        final StringBuilder sb = new StringBuilder();
        //WORKAROUND: sleeping 10 ms between messages is way too slow
        final ASpinWait spinWait = new ASpinWait() {
            @Override
            public boolean isConditionFulfilled() throws Exception {
                if (interruptedCheck.check()) {
                    checkError();
                }
                while (readerForSocket.available() > 0 && !Thread.interrupted()) {
                    final int b = readerForSocket.read();
                    if (b == NEW_LINE) {
                        return true;
                    }
                    sb.append((char) b);
                }
                return false;
            }
        };
        try {
            spinWait.awaitFulfill(System.nanoTime());
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        if (sb.length() == 0) {
            return null;
        }
        final String s = sb.toString();
        return s;
    }

}
