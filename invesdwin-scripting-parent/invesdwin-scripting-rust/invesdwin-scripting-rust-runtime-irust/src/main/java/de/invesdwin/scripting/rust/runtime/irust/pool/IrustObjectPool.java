package de.invesdwin.scripting.rust.runtime.irust.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class IrustObjectPool extends ATimeoutObjectPool<ExtendedIrustBridge>
        implements FactoryBean<IrustObjectPool> {

    public static final IrustObjectPool INSTANCE = new IrustObjectPool();

    private IrustObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedIrustBridge element) {
        element.close();
    }

    @Override
    protected ExtendedIrustBridge newObject() {
        final ExtendedIrustBridge session = new ExtendedIrustBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedIrustBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public IrustObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return IrustObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
