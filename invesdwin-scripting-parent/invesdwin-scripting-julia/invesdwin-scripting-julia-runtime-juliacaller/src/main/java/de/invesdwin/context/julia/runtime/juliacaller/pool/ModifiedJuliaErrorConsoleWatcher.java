package de.invesdwin.context.julia.runtime.juliacaller.pool;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.InputStreamReader;

import javax.annotation.concurrent.GuardedBy;
import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.util.concurrent.Threads;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.time.date.FTimeUnit;

@ThreadSafe
public class ModifiedJuliaErrorConsoleWatcher implements Closeable {

    private final BufferedReader errorReader;
    private final BufferedReader infoReader;

    private volatile Thread errorThread;
    private volatile Thread infoThread;

    @GuardedBy("self")
    private final StringBuilder errorMessage = new StringBuilder();

    public ModifiedJuliaErrorConsoleWatcher(final Process process) {
        this.errorReader = new BufferedReader(new InputStreamReader(process.getErrorStream()));
        this.infoReader = new BufferedReader(new InputStreamReader(process.getInputStream()));
    }

    public void startWatching() {
        errorThread = new Thread(new Runnable() {
            @Override
            public void run() {
                final Thread currentThread = Thread.currentThread();
                try {
                    while (!Threads.isInterrupted() && errorThread == currentThread) {
                        final String s = errorReader.readLine();
                        if (errorThread != currentThread) {
                            return;
                        }
                        if (Strings.isNotBlank(s) && !s.contains("Info: Precompiling ")) {
                            IScriptTaskRunnerJulia.LOG.warn(s);
                            synchronized (errorMessage) {
                                if (errorMessage.length() > 0) {
                                    errorMessage.append("\n");
                                }
                                errorMessage.append(s);
                            }
                        } else {
                            FTimeUnit.MILLISECONDS.sleep(1);
                        }
                    }
                } catch (final Throwable e) {
                    //ignore, process must have been closed
                }
            }
        });
        errorThread.start();
        infoThread = new Thread(new Runnable() {
            @Override
            public void run() {
                final Thread currentThread = Thread.currentThread();
                try {
                    while (!Threads.isInterrupted() && infoThread == currentThread) {
                        final String s = infoReader.readLine();
                        if (infoThread != currentThread) {
                            return;
                        }
                        if (Strings.isNotBlank(s)) {
                            IScriptTaskRunnerJulia.LOG.info(s);
                        } else {
                            FTimeUnit.MILLISECONDS.sleep(1);
                        }
                    }
                } catch (final Throwable e) {
                    //ignore, process must have been closed
                }
            }
        });
        infoThread.start();
    }

    @Override
    public void close() {
        errorThread = null;
        infoThread = null;
        clearLog();
    }

    public void clearLog() {
        synchronized (errorMessage) {
            errorMessage.setLength(0);
        }
    }

    private int getErrorMessageLength() {
        synchronized (errorMessage) {
            return errorMessage.length();
        }
    }

    public String getErrorMessage() {
        int prevLength = 0;
        while (true) {
            final int length = getErrorMessageLength();
            if (length == 0) {
                return null;
            }
            if (length > prevLength) {
                prevLength = length;
                //wait for the whole messsage to arrive
                FTimeUnit.MILLISECONDS.sleepNoInterrupt(50);
            } else {
                break;
            }
        }
        final String str;
        synchronized (errorMessage) {
            if (errorMessage.length() == 0) {
                return null;
            }
            str = String.valueOf(errorMessage).trim();
            errorMessage.setLength(0);
        }
        return str;
    }
}