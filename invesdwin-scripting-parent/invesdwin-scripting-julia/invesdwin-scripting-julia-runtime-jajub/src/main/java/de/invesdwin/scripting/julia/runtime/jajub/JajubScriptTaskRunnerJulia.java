package de.invesdwin.scripting.julia.runtime.jajub;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskJulia;
import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.scripting.julia.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.julia.runtime.jajub.pool.ExtendedJuliaBridge;
import de.invesdwin.scripting.julia.runtime.jajub.pool.JajubObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JajubScriptTaskRunnerJulia
        implements IScriptTaskRunnerJulia, FactoryBean<JajubScriptTaskRunnerJulia> {

    public static final JajubScriptTaskRunnerJulia INSTANCE = new JajubScriptTaskRunnerJulia();

    /**
     * public for ServiceLoader support
     */
    public JajubScriptTaskRunnerJulia() {}

    @Override
    public <T> T run(final AScriptTaskJulia<T> scriptTask) {
        //get session
        final ExtendedJuliaBridge bridge = JajubObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final JajubScriptTaskEngineJulia engine = new JajubScriptTaskEngineJulia(bridge);
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
            JajubObjectPool.INSTANCE.returnObject(bridge);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            JajubObjectPool.INSTANCE.invalidateObject(bridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JajubScriptTaskRunnerJulia getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JajubScriptTaskRunnerJulia.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
