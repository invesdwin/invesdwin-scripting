package de.invesdwin.context.r.runtime.jri;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.r.runtime.contract.AScriptTaskR;
import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.r.runtime.jri.internal.LoggingRMainLoopCallbacks;
import de.invesdwin.context.r.runtime.jri.internal.RengineWrapper;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JriScriptTaskRunnerR implements IScriptTaskRunnerR, FactoryBean<JriScriptTaskRunnerR> {

    public static final JriScriptTaskRunnerR INSTANCE = new JriScriptTaskRunnerR();

    /**
     * public for ServiceLoader support
     */
    public JriScriptTaskRunnerR() {}

    @Override
    public <T> T run(final AScriptTaskR<T> scriptTask) {
        //get session
        final JriScriptTaskEngineR engine = new JriScriptTaskEngineR(RengineWrapper.INSTANCE);
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        final ILock lock = engine.getSharedLock();
        lock.lock();
        try {
            //inputs
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
            return result;
        } catch (final Throwable t) {
            throw Throwables.propagate(t);
        } finally {
            LoggingRMainLoopCallbacks.INSTANCE.reset();
            lock.unlock();
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JriScriptTaskRunnerR getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JriScriptTaskRunnerR.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
