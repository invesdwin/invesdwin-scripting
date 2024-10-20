package de.invesdwin.context.haskell.runtime.frege.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class FregeObjectPool extends ATimeoutObjectPool<ExtendedFregeBridge>
        implements FactoryBean<FregeObjectPool> {

    public static final FregeObjectPool INSTANCE = new FregeObjectPool();

    private FregeObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedFregeBridge element) {
        element.close();
    }

    @Override
    protected ExtendedFregeBridge newObject() {
        final ExtendedFregeBridge session = new ExtendedFregeBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedFregeBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public FregeObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return FregeObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
