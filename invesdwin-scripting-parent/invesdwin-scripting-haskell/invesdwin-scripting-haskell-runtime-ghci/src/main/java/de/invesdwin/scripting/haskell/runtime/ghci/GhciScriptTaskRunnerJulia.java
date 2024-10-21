package de.invesdwin.scripting.haskell.runtime.ghci;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskHaskell;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.scripting.haskell.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.ExtendedGhciBridge;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.GhciObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class GhciScriptTaskRunnerJulia
        implements IScriptTaskRunnerHaskell, FactoryBean<GhciScriptTaskRunnerJulia> {

    public static final GhciScriptTaskRunnerJulia INSTANCE = new GhciScriptTaskRunnerJulia();

    /**
     * public for ServiceLoader support
     */
    public GhciScriptTaskRunnerJulia() {}

    @Override
    public <T> T run(final AScriptTaskHaskell<T> scriptTask) {
        //get session
        final ExtendedGhciBridge bridge = GhciObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final GhciScriptTaskEngineJulia engine = new GhciScriptTaskEngineJulia(bridge);
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
            GhciObjectPool.INSTANCE.returnObject(bridge);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            GhciObjectPool.INSTANCE.invalidateObject(bridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public GhciScriptTaskRunnerJulia getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return GhciScriptTaskRunnerJulia.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
