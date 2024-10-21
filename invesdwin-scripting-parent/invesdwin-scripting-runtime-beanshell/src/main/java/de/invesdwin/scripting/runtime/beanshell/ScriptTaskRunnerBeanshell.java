package de.invesdwin.scripting.runtime.beanshell;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.runtime.beanshell.callback.BeanshellScriptTaskCallbackContext;
import de.invesdwin.scripting.runtime.beanshell.pool.BeanshellScriptEngineObjectPool;
import de.invesdwin.scripting.runtime.beanshell.pool.IBeanshellEngine;
import de.invesdwin.util.concurrent.pool.IObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class ScriptTaskRunnerBeanshell
        implements IScriptTaskRunnerBeanshell, FactoryBean<ScriptTaskRunnerBeanshell> {

    public static final ScriptTaskRunnerBeanshell INSTANCE = new ScriptTaskRunnerBeanshell();

    /**
     * public for ServiceLoader support
     */
    public ScriptTaskRunnerBeanshell() {}

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public <T> T run(final AScriptTaskBeanshell<T> scriptTask) {
        //get session
        final IObjectPool<IBeanshellEngine> pool = (IObjectPool) BeanshellScriptEngineObjectPool.INSTANCE;
        final IBeanshellEngine scriptEngine = pool.borrowObject();
        final BeanshellScriptTaskCallbackContext context;
        final IScriptTaskCallback callback = scriptTask.getCallback();
        if (callback != null) {
            context = new BeanshellScriptTaskCallbackContext(
                    LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final ScriptTaskEngineBeanshell engine = new ScriptTaskEngineBeanshell(scriptEngine);
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
            pool.returnObject(scriptEngine);
            return result;
        } catch (final Throwable t) {
            pool.invalidateObject(scriptEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public ScriptTaskRunnerBeanshell getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return ScriptTaskRunnerBeanshell.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
