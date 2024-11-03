package de.invesdwin.scripting.rust.runtime.evcxr.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class EvcxrObjectPool extends ATimeoutObjectPool<ExtendedEvcxrBridge>
        implements FactoryBean<EvcxrObjectPool> {

    public static final EvcxrObjectPool INSTANCE = new EvcxrObjectPool();

    private EvcxrObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedEvcxrBridge element) {
        element.close();
    }

    @Override
    protected ExtendedEvcxrBridge newObject() {
        final ExtendedEvcxrBridge session = new ExtendedEvcxrBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedEvcxrBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public EvcxrObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return EvcxrObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
