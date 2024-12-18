package de.invesdwin.scripting.haskell.runtime.frege;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskHaskell;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.scripting.haskell.runtime.frege.callback.file.FileScriptTaskCallbackContext;
import de.invesdwin.scripting.haskell.runtime.frege.pool.ExtendedFregeBridge;
import de.invesdwin.scripting.haskell.runtime.frege.pool.FregeObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class FregeScriptTaskRunnerHaskell
        implements IScriptTaskRunnerHaskell, FactoryBean<FregeScriptTaskRunnerHaskell> {

    public static final FregeScriptTaskRunnerHaskell INSTANCE = new FregeScriptTaskRunnerHaskell();

    /**
     * public for ServiceLoader support
     */
    public FregeScriptTaskRunnerHaskell() {}

    @Override
    public <T> T run(final AScriptTaskHaskell<T> scriptTask) {
        //get session
        final ExtendedFregeBridge bridge = FregeObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final FileScriptTaskCallbackContext context;
        if (callback != null) {
            context = new FileScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final FregeScriptTaskEngineHaskell engine = new FregeScriptTaskEngineHaskell(bridge);
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
            FregeObjectPool.INSTANCE.returnObject(bridge);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            FregeObjectPool.INSTANCE.invalidateObject(bridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public FregeScriptTaskRunnerHaskell getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return FregeScriptTaskRunnerHaskell.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
