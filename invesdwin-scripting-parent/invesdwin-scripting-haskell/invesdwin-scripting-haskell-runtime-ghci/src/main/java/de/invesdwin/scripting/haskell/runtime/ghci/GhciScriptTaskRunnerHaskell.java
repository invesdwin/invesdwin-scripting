package de.invesdwin.scripting.haskell.runtime.ghci;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskHaskell;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.scripting.haskell.runtime.ghci.callback.file.FileScriptTaskCallbackContext;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.ExtendedGhciBridge;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.GhciObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class GhciScriptTaskRunnerHaskell
        implements IScriptTaskRunnerHaskell, FactoryBean<GhciScriptTaskRunnerHaskell> {

    public static final GhciScriptTaskRunnerHaskell INSTANCE = new GhciScriptTaskRunnerHaskell();

    /**
     * public for ServiceLoader support
     */
    public GhciScriptTaskRunnerHaskell() {}

    @Override
    public <T> T run(final AScriptTaskHaskell<T> scriptTask) {
        //get session
        final ExtendedGhciBridge bridge = GhciObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final FileScriptTaskCallbackContext context;
        if (callback != null) {
            context = new FileScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final GhciScriptTaskEngineHaskell engine = new GhciScriptTaskEngineHaskell(bridge);
            if (context != null) {
                context.init(engine);
            }
            scriptTask.populateInputs(engine.getInputs());

            //execute
            scriptTask.executeScript(engine);

            //results
            final T result = scriptTask.extractResults(engine.getResults());
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
    public GhciScriptTaskRunnerHaskell getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return GhciScriptTaskRunnerHaskell.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
