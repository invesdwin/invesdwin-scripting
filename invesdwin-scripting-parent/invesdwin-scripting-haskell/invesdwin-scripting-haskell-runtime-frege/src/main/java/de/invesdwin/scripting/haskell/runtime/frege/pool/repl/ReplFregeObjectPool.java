package de.invesdwin.scripting.haskell.runtime.frege.pool.repl;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class ReplFregeObjectPool extends ATimeoutObjectPool<ReplFregeBridge>
        implements FactoryBean<ReplFregeObjectPool> {

    public static final ReplFregeObjectPool INSTANCE = new ReplFregeObjectPool();

    private ReplFregeObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ReplFregeBridge element) {
        element.close();
    }

    @Override
    protected ReplFregeBridge newObject() {
        final ReplFregeBridge session = new ReplFregeBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ReplFregeBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ReplFregeObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return ReplFregeObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
