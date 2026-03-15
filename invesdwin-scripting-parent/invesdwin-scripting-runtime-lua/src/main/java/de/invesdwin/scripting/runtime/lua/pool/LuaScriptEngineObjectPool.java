package de.invesdwin.scripting.runtime.lua.pool;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.AInvalidatingObjectPool;
import jakarta.inject.Named;

/**
 * We need to always invalidate, otherwise the classloader that keeps the class per statement gets full and each
 * internal map access grinds to a halt. Thus it is cheaper to NOT reuse jshell instances.
 */
@ThreadSafe
@Named
public final class LuaScriptEngineObjectPool extends AInvalidatingObjectPool<WrappedLuaScriptEngine>
        implements FactoryBean<LuaScriptEngineObjectPool> {

    public static final LuaScriptEngineObjectPool INSTANCE = new LuaScriptEngineObjectPool();

    private LuaScriptEngineObjectPool() {
        super();
    }

    @Override
    public void invalidateObject(final WrappedLuaScriptEngine obj) {
        obj.close();
    }

    @Override
    protected WrappedLuaScriptEngine newObject() {
        return new WrappedLuaScriptEngine();
    }

    @Override
    public LuaScriptEngineObjectPool getObject() {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return LuaScriptEngineObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
