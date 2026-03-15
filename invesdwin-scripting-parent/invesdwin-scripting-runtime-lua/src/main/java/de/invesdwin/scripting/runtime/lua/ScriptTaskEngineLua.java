package de.invesdwin.scripting.runtime.lua;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool;
import de.invesdwin.scripting.runtime.lua.pool.WrappedLuaScriptEngine;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class ScriptTaskEngineLua implements IScriptTaskEngine {

    private WrappedLuaScriptEngine scriptEngine;
    private final ScriptTaskInputsLua inputs;
    private final ScriptTaskResultsLua results;

    public ScriptTaskEngineLua(final WrappedLuaScriptEngine scriptEngine) {
        this.scriptEngine = scriptEngine;
        this.inputs = new ScriptTaskInputsLua(this);
        this.results = new ScriptTaskResultsLua(this);
    }

    @Override
    public void eval(final String expression) {
        scriptEngine.eval(expression);
    }

    @Override
    public ScriptTaskInputsLua getInputs() {
        return inputs;
    }

    @Override
    public ScriptTaskResultsLua getResults() {
        return results;
    }

    @Override
    public void close() {
        scriptEngine = null;
    }

    @Override
    public WrappedLuaScriptEngine unwrap() {
        return scriptEngine;
    }

    /**
     * Each instance has its own engine, so no shared locking required.
     */
    @Override
    public ILock getSharedLock() {
        return DisabledLock.INSTANCE;
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static ScriptTaskEngineLua newInstance() {
        return new ScriptTaskEngineLua(LuaScriptEngineObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final WrappedLuaScriptEngine unwrap = unwrap();
                if (unwrap != null) {
                    LuaScriptEngineObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
