package de.invesdwin.scripting.rust.runtime.rhai.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class RhaiObjectPool extends ATimeoutObjectPool<ExtendedRhaiBridge>
        implements FactoryBean<RhaiObjectPool> {

    public static final RhaiObjectPool INSTANCE = new RhaiObjectPool();

    private RhaiObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedRhaiBridge element) {
        element.close();
    }

    @Override
    protected ExtendedRhaiBridge newObject() {
        final ExtendedRhaiBridge session = new ExtendedRhaiBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedRhaiBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public RhaiObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RhaiObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
