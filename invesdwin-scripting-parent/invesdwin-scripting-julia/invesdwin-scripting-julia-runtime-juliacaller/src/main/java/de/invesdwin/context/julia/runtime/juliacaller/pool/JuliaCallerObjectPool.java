package de.invesdwin.context.julia.runtime.juliacaller.pool;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.network.NetworkUtil;
import de.invesdwin.context.julia.runtime.juliacaller.JuliaCallerProperties;
import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class JuliaCallerObjectPool extends ATimeoutObjectPool<ExtendedJuliaCaller>
        implements FactoryBean<JuliaCallerObjectPool> {

    public static final JuliaCallerObjectPool INSTANCE = new JuliaCallerObjectPool();

    private JuliaCallerObjectPool() {
        //julia compilation is a lot of overhead, thus keep instances open longer
        super(new Duration(10, FTimeUnit.MINUTES), new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedJuliaCaller element) {
        try {
            element.shutdownServer();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected ExtendedJuliaCaller newObject() {
        final int port = NetworkUtil.findAvailableTcpPort();
        final ExtendedJuliaCaller session = new ExtendedJuliaCaller(JuliaCallerProperties.JULIA_COMMAND, port);
        try {
            session.startServer();
            session.connect();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    protected boolean passivateObject(final ExtendedJuliaCaller element) {
        try {
            element.reset();
            return true;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JuliaCallerObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JuliaCallerObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
