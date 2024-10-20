package de.invesdwin.context.r.runtime.rcaller;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import com.github.rcaller.rstuff.RCaller;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.r.runtime.contract.AScriptTaskR;
import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.r.runtime.rcaller.pool.RCallerObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class RCallerScriptTaskRunnerR implements IScriptTaskRunnerR, FactoryBean<RCallerScriptTaskRunnerR> {

    public static final RCallerScriptTaskRunnerR INSTANCE = new RCallerScriptTaskRunnerR();

    public static final String INTERNAL_RESULT_VARIABLE = RCallerScriptTaskRunnerR.class.getSimpleName() + "_result";

    /**
     * public for ServiceLoader support
     */
    public RCallerScriptTaskRunnerR() {}

    @Override
    public <T> T run(final AScriptTaskR<T> scriptTask) {
        //get session
        final RCaller rcaller = RCallerObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            rcaller.getRCode().clearOnline();
            final RCallerScriptTaskEngineR engine = new RCallerScriptTaskEngineR(rcaller);
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
            RCallerObjectPool.INSTANCE.returnObject(rcaller);
            return result;
        } catch (final Throwable t) {
            RCallerObjectPool.INSTANCE.invalidateObject(rcaller);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public RCallerScriptTaskRunnerR getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RCallerScriptTaskRunnerR.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
