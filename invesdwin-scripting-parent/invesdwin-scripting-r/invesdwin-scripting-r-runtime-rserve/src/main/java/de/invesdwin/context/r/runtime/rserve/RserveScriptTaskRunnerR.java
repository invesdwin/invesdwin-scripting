package de.invesdwin.context.r.runtime.rserve;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.r.runtime.contract.AScriptTaskR;
import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.r.runtime.rserve.pool.ExtendedRserveSession;
import de.invesdwin.context.r.runtime.rserve.pool.RsessionObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class RserveScriptTaskRunnerR implements IScriptTaskRunnerR, FactoryBean<RserveScriptTaskRunnerR> {

    public static final RserveScriptTaskRunnerR INSTANCE = new RserveScriptTaskRunnerR();

    /**
     * public for ServiceLoader support
     */
    public RserveScriptTaskRunnerR() {}

    @Override
    public <T> T run(final AScriptTaskR<T> scriptTask) {
        //get session
        final ExtendedRserveSession rsession = RsessionObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final RserveScriptTaskEngineR engine = new RserveScriptTaskEngineR(rsession);
            if (context != null) {
                context.init(engine);
            }
            scriptTask.populateInputs(engine.getInputs());

            //execute
            scriptTask.executeScript(engine);

            //results
            final T result = scriptTask.extractResults(engine.getResults());
            if (context != null) {
                context.deinit(engine);
            }
            engine.close();

            //return
            RsessionObjectPool.INSTANCE.returnObject(rsession);
            return result;
        } catch (final Throwable t) {
            RsessionObjectPool.INSTANCE.invalidateObject(rsession);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public RserveScriptTaskRunnerR getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RserveScriptTaskRunnerR.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
