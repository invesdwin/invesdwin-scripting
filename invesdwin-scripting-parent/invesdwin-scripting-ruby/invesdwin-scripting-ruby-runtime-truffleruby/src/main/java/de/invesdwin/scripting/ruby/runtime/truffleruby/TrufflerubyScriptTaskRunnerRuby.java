package de.invesdwin.scripting.ruby.runtime.truffleruby;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.ruby.runtime.AScriptTaskRuby;
import de.invesdwin.scripting.ruby.runtime.IScriptTaskRunnerRuby;
import de.invesdwin.scripting.ruby.runtime.truffleruby.callback.TrufflerubyScriptTaskCallbackContext;
import de.invesdwin.scripting.ruby.runtime.truffleruby.pool.TrufflerubyScriptEngineObjectPool;
import de.invesdwin.scripting.ruby.runtime.truffleruby.pool.WrappedTrufflerubyScriptEngine;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class TrufflerubyScriptTaskRunnerRuby
        implements IScriptTaskRunnerRuby, FactoryBean<TrufflerubyScriptTaskRunnerRuby> {

    public static final TrufflerubyScriptTaskRunnerRuby INSTANCE = new TrufflerubyScriptTaskRunnerRuby();

    /**
     * public for ServiceLoader support
     */
    public TrufflerubyScriptTaskRunnerRuby() {}

    @Override
    public <T> T run(final AScriptTaskRuby<T> scriptTask) {
        //get session
        final WrappedTrufflerubyScriptEngine scriptEngine = TrufflerubyScriptEngineObjectPool.INSTANCE.borrowObject();
        final TrufflerubyScriptTaskCallbackContext context;
        final IScriptTaskCallback callback = scriptTask.getCallback();
        if (callback != null) {
            context = new TrufflerubyScriptTaskCallbackContext(
                    LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final TrufflerubyScriptTaskEngineRuby engine = new TrufflerubyScriptTaskEngineRuby(scriptEngine);
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
            TrufflerubyScriptEngineObjectPool.INSTANCE.returnObject(scriptEngine);
            return result;
        } catch (final Throwable t) {
            TrufflerubyScriptEngineObjectPool.INSTANCE.invalidateObject(scriptEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public TrufflerubyScriptTaskRunnerRuby getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return TrufflerubyScriptTaskRunnerRuby.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
