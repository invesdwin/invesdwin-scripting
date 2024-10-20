package de.invesdwin.context.matlab.runtime.javasci.internal;

import javax.annotation.concurrent.GuardedBy;
import javax.annotation.concurrent.NotThreadSafe;

import org.scilab.modules.javasci.Scilab;

import de.invesdwin.context.log.error.Err;
import de.invesdwin.context.matlab.runtime.javasci.JavasciProperties;
import de.invesdwin.util.concurrent.lock.IReentrantLock;
import de.invesdwin.util.concurrent.lock.Locks;

/**
 * Always acquire the lock first before accessing the scilab instance.
 */
@NotThreadSafe
public final class ScilabWrapper {

    public static final ScilabWrapper INSTANCE = new ScilabWrapper();

    @GuardedBy("lock")
    private final Scilab scilab;
    private final IReentrantLock lock;

    private ScilabWrapper() {
        try {
            scilab = new Scilab(JavasciProperties.SCILAB_PATH, false);
            scilab.open();
        } catch (final Exception e) {
            throw Err.process(e);
        }
        lock = Locks.newReentrantLock(ScilabWrapper.class.getSimpleName() + "_lock");
    }

    public Scilab getScilab() {
        return scilab;
    }

    public IReentrantLock getLock() {
        return lock;
    }

}
