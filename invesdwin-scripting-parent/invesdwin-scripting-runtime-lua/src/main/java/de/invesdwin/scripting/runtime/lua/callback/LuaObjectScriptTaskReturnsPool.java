package de.invesdwin.scripting.runtime.lua.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class LuaObjectScriptTaskReturnsPool extends AAgronaObjectPool<LuaObjectScriptTaskReturns> {

    public static final LuaObjectScriptTaskReturnsPool INSTANCE = new LuaObjectScriptTaskReturnsPool();

    private LuaObjectScriptTaskReturnsPool() {}

    @Override
    protected LuaObjectScriptTaskReturns newObject() {
        return new LuaObjectScriptTaskReturns();
    }

    @Override
    protected boolean passivateObject(final LuaObjectScriptTaskReturns element) {
        element.close();
        return true;
    }

}
