package de.invesdwin.scripting.ruby.runtime.jruby;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.ruby.runtime.AScriptTaskRuby;
import de.invesdwin.scripting.ruby.runtime.IScriptTaskRunnerRuby;
import de.invesdwin.scripting.ruby.runtime.jruby.callback.JrubyScriptTaskCallbackContext;
import de.invesdwin.scripting.ruby.runtime.jruby.pool.JrubyScriptEngineObjectPool;
import de.invesdwin.scripting.ruby.runtime.jruby.pool.WrappedJrubyScriptEngine;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JrubyScriptTaskRunnerRuby implements IScriptTaskRunnerRuby, FactoryBean<JrubyScriptTaskRunnerRuby> {

    public static final JrubyScriptTaskRunnerRuby INSTANCE = new JrubyScriptTaskRunnerRuby();

    /**
     * public for ServiceLoader support
     */
    public JrubyScriptTaskRunnerRuby() {}

    @Override
    public <T> T run(final AScriptTaskRuby<T> scriptTask) {
        //get session
        final WrappedJrubyScriptEngine scriptEngine = JrubyScriptEngineObjectPool.INSTANCE.borrowObject();
        final JrubyScriptTaskCallbackContext context;
        final IScriptTaskCallback callback = scriptTask.getCallback();
        if (callback != null) {
            context = new JrubyScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final JrubyScriptTaskEngineRuby engine = new JrubyScriptTaskEngineRuby(scriptEngine);
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
            JrubyScriptEngineObjectPool.INSTANCE.returnObject(scriptEngine);
            return result;
        } catch (final Throwable t) {
            JrubyScriptEngineObjectPool.INSTANCE.invalidateObject(scriptEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JrubyScriptTaskRunnerRuby getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JrubyScriptTaskRunnerRuby.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
