package de.invesdwin.context.r.runtime.rserve.pool;

import java.io.File;

import javax.annotation.concurrent.ThreadSafe;

import org.math.R.Log;
import org.math.R.RserveAccessor;
import org.math.R.RserveDaemon;
import org.math.R.RserverConf;
import org.math.R.StartRserve;
import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.network.NetworkUtil;
import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.rserve.RserveProperties;
import de.invesdwin.context.r.runtime.rserve.RserveScriptTaskEngineR;
import de.invesdwin.context.r.runtime.rserve.RserverConfMode;
import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.error.UnknownArgumentException;
import de.invesdwin.util.lang.reflection.Reflections;
import de.invesdwin.util.lang.string.UniqueNameGenerator;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class RsessionObjectPool extends ATimeoutObjectPool<ExtendedRserveSession>
        implements FactoryBean<RsessionObjectPool> {

    static {
        Log.Out = new Log() {

            @Override
            public void println(final String s) {
                //noop
            }

            @Override
            public void print(final String s) {
                //noop
            }
        };
        Log.Err = new Log() {

            @Override
            public void println(final String s) {
                //noop
            }

            @Override
            public void print(final String s) {
                //noop
            }
        };
    }

    public static final RsessionObjectPool INSTANCE = new RsessionObjectPool();
    private static final UniqueNameGenerator UNIQUE_NAME_GENERATOR = new UniqueNameGenerator();

    private RsessionObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final ExtendedRserveSession element) {
        element.end();
    }

    @Override
    protected ExtendedRserveSession newObject() {
        final ExtendedRserveSession session = createSession();
        /*
         * use absolute path or else connection might not work properly, also use unique name to prevent multiple
         * instances from sharing the same file
         */
        final String sinkFile = new File(ContextProperties.TEMP_DIRECTORY, UNIQUE_NAME_GENERATOR.get(".Rout"))
                .getAbsolutePath();
        Reflections.field("SINK_FILE").ofType(String.class).in(session).set(sinkFile);
        //https://github.com/yannrichet/rsession/issues/21 not using sink is a lot faster, errors are still properly reported
        session.sinkOutput(false);
        session.sinkMessage(false);
        return session;
    }

    private ExtendedRserveSession createSession() {
        final de.invesdwin.context.r.runtime.rserve.pool.internal.RsessionLogger log = new de.invesdwin.context.r.runtime.rserve.pool.internal.RsessionLogger();
        switch (RserveProperties.RSERVER_CONF_MODE) {
        case LOCAL_SPAWN:
            try {
                if (RserveProperties.RSERVER_CONF.port <= 0) {
                    final RserverConf conf = new RserverConf(RserveProperties.RSERVER_CONF.host,
                            NetworkUtil.findAvailableTcpPort(RserverConf.DEFAULT_RSERVE_PORT),
                            RserveProperties.RSERVER_CONF.login, RserveProperties.RSERVER_CONF.password);
                    final RserveDaemon daemon = new RserveDaemon(conf, log);
                    daemon.start();
                    final ExtendedRserveSession session = new ExtendedRserveSession(log, null,
                            RserveAccessor.getConf(daemon));
                    RserveAccessor.setLocalRServe(session, daemon);
                    return session;
                } else {
                    final RserverConf conf = RserveProperties.RSERVER_CONF;
                    synchronized (StartRserve.class) {
                        if (!StartRserve.isRserveListening(conf.port)) {
                            final RserveDaemon daemon = new RserveDaemon(conf, log);
                            daemon.start();
                        }
                    }
                    return new ExtendedRserveSession(log, null, conf);
                }
            } catch (final Exception e) {
                throw new RuntimeException(e);
            }
        case LOCAL:
            //fallthrough
        case REMOTE:
            return new ExtendedRserveSession(log, null, RserveProperties.RSERVER_CONF);
        default:
            throw UnknownArgumentException.newInstance(RserverConfMode.class, RserveProperties.RSERVER_CONF_MODE);
        }
    }

    @Override
    protected boolean passivateObject(final ExtendedRserveSession element) {
        final RserveScriptTaskEngineR engine = new RserveScriptTaskEngineR(element);
        engine.eval(IScriptTaskRunnerR.CLEANUP_SCRIPT);
        engine.close();
        element.closeLog(); //reset logger
        return true;
    }

    @Override
    public RsessionObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RsessionObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
