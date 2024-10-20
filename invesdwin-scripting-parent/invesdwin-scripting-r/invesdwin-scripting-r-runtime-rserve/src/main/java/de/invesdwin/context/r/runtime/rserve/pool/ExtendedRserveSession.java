package de.invesdwin.context.r.runtime.rserve.pool;

import java.util.Properties;

import javax.annotation.concurrent.NotThreadSafe;

import org.math.R.RLog;
import org.math.R.RserveSession;
import org.math.R.RserverConf;
import org.rosuda.REngine.REXP;

import de.invesdwin.context.r.runtime.rserve.RserveProperties;

@NotThreadSafe
public class ExtendedRserveSession extends RserveSession {

    public ExtendedRserveSession(final RLog log, final Properties properties, final RserverConf serverconf) {
        super(log, properties, serverconf);
        setCRANRepository(RserveProperties.RSERVER_REPOSITORY);
    }

    @Override
    public REXP rawEval(final String expression) {
        return (REXP) super.rawEval(expression);
    }

    @Override
    public REXP rawEval(final String expression, final boolean tryEval) {
        final Object result = super.rawEval(expression, tryEval);
        if (result instanceof RException) {
            final RException cResult = (RException) result;
            throw new RuntimeException(cResult);
        }
        return (REXP) result;
    }
}
