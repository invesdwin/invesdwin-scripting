package de.invesdwin.scripting.julia.runtime.juliacaller;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskJulia;
import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.scripting.julia.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.julia.runtime.juliacaller.pool.ExtendedJuliaCaller;
import de.invesdwin.scripting.julia.runtime.juliacaller.pool.JuliaCallerObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JuliaCallerScriptTaskRunnerJulia
        implements IScriptTaskRunnerJulia, FactoryBean<JuliaCallerScriptTaskRunnerJulia> {

    public static final JuliaCallerScriptTaskRunnerJulia INSTANCE = new JuliaCallerScriptTaskRunnerJulia();

    /**
     * public for ServiceLoader support
     */
    public JuliaCallerScriptTaskRunnerJulia() {}

    @Override
    public <T> T run(final AScriptTaskJulia<T> scriptTask) {
        //get session
        final ExtendedJuliaCaller juliaCaller = JuliaCallerObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final JuliaCallerScriptTaskEngineJulia engine = new JuliaCallerScriptTaskEngineJulia(juliaCaller);
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
            JuliaCallerObjectPool.INSTANCE.returnObject(juliaCaller);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            JuliaCallerObjectPool.INSTANCE.invalidateObject(juliaCaller);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JuliaCallerScriptTaskRunnerJulia getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JuliaCallerScriptTaskRunnerJulia.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
