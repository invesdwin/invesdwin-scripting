package de.invesdwin.scripting.rust.runtime.rhai;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskRust;
import de.invesdwin.scripting.rust.runtime.contract.IScriptTaskRunnerRust;
import de.invesdwin.scripting.rust.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.rust.runtime.rhai.pool.ExtendedRhaiBridge;
import de.invesdwin.scripting.rust.runtime.rhai.pool.RhaiObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class RhaiScriptTaskRunnerRust
        implements IScriptTaskRunnerRust, FactoryBean<RhaiScriptTaskRunnerRust> {

    public static final RhaiScriptTaskRunnerRust INSTANCE = new RhaiScriptTaskRunnerRust();

    /**
     * public for ServiceLoader support
     */
    public RhaiScriptTaskRunnerRust() {}

    @Override
    public <T> T run(final AScriptTaskRust<T> scriptTask) {
        //get session
        final ExtendedRhaiBridge bridge = RhaiObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final RhaiScriptTaskEngineRust engine = new RhaiScriptTaskEngineRust(bridge);
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
            RhaiObjectPool.INSTANCE.returnObject(bridge);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            RhaiObjectPool.INSTANCE.invalidateObject(bridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public RhaiScriptTaskRunnerRust getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RhaiScriptTaskRunnerRust.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
