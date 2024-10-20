package de.invesdwin.context.julia.runtime.jajub.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class JajubObjectPool extends ATimeoutObjectPool<ExtendedJuliaBridge>
        implements FactoryBean<JajubObjectPool> {

    public static final JajubObjectPool INSTANCE = new JajubObjectPool();

    private JajubObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedJuliaBridge element) {
        element.close();
    }

    @Override
    protected ExtendedJuliaBridge newObject() {
        final ExtendedJuliaBridge session = new ExtendedJuliaBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedJuliaBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JajubObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JajubObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
