package de.invesdwin.scripting.runtime.lua;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext;
import de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool;
import de.invesdwin.scripting.runtime.lua.pool.WrappedLuaScriptEngine;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class ScriptTaskRunnerLua implements IScriptTaskRunnerLua, FactoryBean<ScriptTaskRunnerLua> {

    public static final ScriptTaskRunnerLua INSTANCE = new ScriptTaskRunnerLua();

    /**
     * public for ServiceLoader support
     */
    public ScriptTaskRunnerLua() {}

    @Override
    public <T> T run(final AScriptTaskLua<T> scriptTask) {
        //get session
        final WrappedLuaScriptEngine scriptEngine = LuaScriptEngineObjectPool.INSTANCE.borrowObject();
        final LuaScriptTaskCallbackContext context;
        final IScriptTaskCallback callback = scriptTask.getCallback();
        if (callback != null) {
            context = new LuaScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final ScriptTaskEngineLua engine = new ScriptTaskEngineLua(scriptEngine);
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
            LuaScriptEngineObjectPool.INSTANCE.returnObject(scriptEngine);
            return result;
        } catch (final Throwable t) {
            LuaScriptEngineObjectPool.INSTANCE.invalidateObject(scriptEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public ScriptTaskRunnerLua getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return ScriptTaskRunnerLua.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
