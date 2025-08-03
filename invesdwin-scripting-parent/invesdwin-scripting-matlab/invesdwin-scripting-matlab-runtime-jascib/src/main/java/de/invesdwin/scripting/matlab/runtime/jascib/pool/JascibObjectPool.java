package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class JascibObjectPool extends ATimeoutObjectPool<ExtendedScilabBridge>
        implements FactoryBean<JascibObjectPool> {

    public static final JascibObjectPool INSTANCE = new JascibObjectPool();

    private JascibObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedScilabBridge element) {
        element.close();
    }

    @Override
    protected ExtendedScilabBridge newObject() {
        final ExtendedScilabBridge session = new ExtendedScilabBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedScilabBridge element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JascibObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JascibObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
