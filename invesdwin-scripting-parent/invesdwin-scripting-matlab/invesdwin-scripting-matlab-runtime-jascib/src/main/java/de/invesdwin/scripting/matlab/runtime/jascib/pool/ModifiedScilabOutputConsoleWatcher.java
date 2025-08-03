package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import java.io.Closeable;
import java.io.InputStream;

import javax.annotation.concurrent.GuardedBy;
import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.collections.circular.CircularByteBuffer;
import de.invesdwin.util.concurrent.Threads;
import de.invesdwin.util.lang.Closeables;
import de.invesdwin.util.time.date.FTimeUnit;

/**
 * pty4j does not offer a non-blocking read option, no timeout and does not offer available() method, thus we have to
 * use a separate thread where we can use blocking IO to read input stream without stalling the main application.
 */
@ThreadSafe
public class ModifiedScilabOutputConsoleWatcher implements Closeable {

    private volatile Thread inputThread;
    private final InputStream inp;

    @GuardedBy("self")
    private final CircularByteBuffer buffer = new CircularByteBuffer();

    public ModifiedScilabOutputConsoleWatcher(final Process process) {
        this.inp = process.getInputStream();
    }

    public void startWatching() {
        inputThread = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    while (!Threads.isInterrupted() && inputThread != null) {
                        if (isFull()) {
                            FTimeUnit.MILLISECONDS.sleep(1);
                        } else {
                            final int b = inp.read();
                            if (inputThread == null) {
                                return;
                            }
                            if (b != -1) {
                                synchronized (buffer) {
                                    buffer.write((byte) b);
                                }
                            } else {
                                FTimeUnit.MILLISECONDS.sleep(1);
                            }
                        }
                    }
                } catch (final Throwable e) {
                    //ignore, process must have been closed
                } finally {
                    Closeables.closeQuietly(inp);
                }
            }
        });
        inputThread.start();
    }

    @Override
    public void close() {
        if (inputThread != null) {
            inputThread.interrupt();
            inputThread = null;
        }
        clearLog();
    }

    public void clearLog() {
        synchronized (buffer) {
            buffer.clear();
        }
    }

    public int available() {
        synchronized (buffer) {
            return buffer.size();
        }
    }

    public boolean isFull() {
        synchronized (buffer) {
            return buffer.isFull();
        }
    }

    public int read() {
        synchronized (buffer) {
            if (buffer.isEmpty()) {
                return -1;
            } else {
                return buffer.read();
            }
        }
    }
}