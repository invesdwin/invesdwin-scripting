package de.invesdwin.scripting.runtime.clojure;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.runtime.clojure.callback.ClojureScriptTaskCallbackContext;
import de.invesdwin.scripting.runtime.clojure.pool.ClojureEngineObjectPool;
import de.invesdwin.scripting.runtime.clojure.pool.WrappedClojureEngine;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class ScriptTaskRunnerClojure implements IScriptTaskRunnerClojure, FactoryBean<ScriptTaskRunnerClojure> {

    public static final ScriptTaskRunnerClojure INSTANCE = new ScriptTaskRunnerClojure();

    /**
     * public for ServiceLoader support
     */
    public ScriptTaskRunnerClojure() {}

    @Override
    public <T> T run(final AScriptTaskClojure<T> scriptTask) {
        //get session
        final WrappedClojureEngine scriptEngine = ClojureEngineObjectPool.INSTANCE.borrowObject();
        final ClojureScriptTaskCallbackContext context;
        final IScriptTaskCallback callback = scriptTask.getCallback();
        if (callback != null) {
            context = new ClojureScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final ScriptTaskEngineClojure engine = new ScriptTaskEngineClojure(scriptEngine);
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
            ClojureEngineObjectPool.INSTANCE.returnObject(scriptEngine);
            return result;
        } catch (final Throwable t) {
            ClojureEngineObjectPool.INSTANCE.invalidateObject(scriptEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public ScriptTaskRunnerClojure getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return ScriptTaskRunnerClojure.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
