package de.invesdwin.scripting.haskell.runtime.ghci.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class GhciObjectPool extends ATimeoutObjectPool<ExtendedGhciBridge>
        implements FactoryBean<GhciObjectPool> {

    public static final GhciObjectPool INSTANCE = new GhciObjectPool();

    private GhciObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedGhciBridge element) {
        element.close();
    }

    @Override
    protected ExtendedGhciBridge newObject() {
        final ExtendedGhciBridge session = new ExtendedGhciBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedGhciBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public GhciObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return GhciObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
