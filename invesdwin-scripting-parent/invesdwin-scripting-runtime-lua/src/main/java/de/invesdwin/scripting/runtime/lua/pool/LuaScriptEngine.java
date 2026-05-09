package de.invesdwin.scripting.runtime.lua.pool;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import de.invesdwin.util.collections.Collections;
import de.invesdwin.util.lang.finalizer.AFinalizer;
import party.iroiro.luajava.ClassPathLoader;
import party.iroiro.luajava.Consts;
import party.iroiro.luajava.Lua;
import party.iroiro.luajava.LuaException;
import party.iroiro.luajava.util.ClassUtils;
import party.iroiro.luajava.value.LuaValue;

/**
 * Adapted from party.iroiro.luajava.jsr223.LuaScriptEngine
 * 
 * This implementation preserves variables between invocations.
 */
@NotThreadSafe
public final class LuaScriptEngine extends AbstractScriptEngine implements ScriptEngine, Compilable, Closeable {
    private final String luaClass;
    private final LuaScriptEngineFactory factory;
    private final LuaScriptEngineFinalizer finalizer;
    private final LuaScriptEngineBindings bindings = new LuaScriptEngineBindings();

    LuaScriptEngine(final String luaClass, final LuaScriptEngineFactory factory) {
        this.luaClass = luaClass;
        this.factory = factory;
        this.finalizer = new LuaScriptEngineFinalizer();
        finalizer.register(this);
    }

    public Lua getLua() {
        Lua luaCopy = finalizer.lua;
        if (luaCopy == null) {
            luaCopy = newLua();
            finalizer.lua = luaCopy;
        }
        return luaCopy;
    }

    private Lua newLua() {
        try {
            final Lua l = (Lua) ClassUtils.forName(luaClass).getDeclaredConstructor().newInstance();
            l.setExternalLoader(new ClassPathLoader());
            l.openLibraries();
            return l;
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void putContext(final Lua l, final ScriptContext scriptContext) {
        final Bindings global = scriptContext.getBindings(ScriptContext.GLOBAL_SCOPE);
        if (global != null && !global.isEmpty()) {
            for (final Entry<String, Object> entry : global.entrySet()) {
                putGlobal(l, entry.getKey(), entry.getValue());
            }
        }
        final Bindings engine = scriptContext.getBindings(ScriptContext.ENGINE_SCOPE);
        if (engine != null && !engine.isEmpty()) {
            for (final Entry<String, Object> entry : engine.entrySet()) {
                putGlobal(l, entry.getKey(), entry.getValue());
            }
        }
    }

    private void putGlobal(final Lua l, final String k, final Object v) {
        l.push(v, Lua.Conversion.SEMI);
        l.setGlobal(k);
    }

    @Override
    public LuaValue[] eval(final String script) throws ScriptException {
        try {
            final Lua l = getLua();
            final LuaValue[] values = l.eval(script);
            return values.length == 0 ? null : values;
        } catch (final LuaException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public LuaValue[] eval(final String script, final ScriptContext scriptContext) throws ScriptException {
        try {
            final Lua l = getLua();
            putContext(l, scriptContext);
            final LuaValue[] values = l.eval(script);
            return values.length == 0 ? null : values;
        } catch (final LuaException e) {
            throw new ScriptException(e);
        }
    }

    public void run(final String script) throws ScriptException {
        try {
            final Lua l = getLua();
            l.run(script);
        } catch (final LuaException e) {
            throw new ScriptException(e);
        }
    }

    public void run(final String script, final ScriptContext scriptContext) throws ScriptException {
        try {
            final Lua l = getLua();
            putContext(l, scriptContext);
            l.run(script);
        } catch (final LuaException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public LuaValue[] eval(final Reader reader, final ScriptContext scriptContext) throws ScriptException {
        return eval(readAll(reader), scriptContext);
    }

    private String readAll(final Reader reader) throws ScriptException {
        final BufferedReader bufferedReader = new BufferedReader(reader);
        final String code = bufferedReader.lines().collect(Collectors.joining("\n"));
        try {
            bufferedReader.close();
        } catch (final IOException e) {
            throw new ScriptException(e);
        }
        return code;
    }

    @Override
    public Bindings createBindings() {
        return bindings;
    }

    @Override
    public Bindings getBindings(final int scope) {
        return bindings;
    }

    @Override
    public void put(final String key, final Object value) {
        final Lua l = getLua();
        putGlobal(l, key, value);
    }

    @Override
    public LuaValue[] get(final String key) {
        try {
            return eval("return " + key);
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ScriptEngineFactory getFactory() {
        return factory;
    }

    @Override
    public CompiledScript compile(final String s) throws ScriptException {
        try {
            final Lua l = getLua();
            l.load(s);
            final java.nio.ByteBuffer dump = l.dump();
            return new CompiledScript() {
                @Override
                public Object eval(final ScriptContext scriptContext) throws ScriptException {
                    try {
                        final Lua l = getLua();
                        putContext(l, scriptContext);
                        final int top = l.getTop();
                        l.load(dump, "CompiledScript");
                        l.pCall(0, Consts.LUA_MULTRET);
                        final int returnCount = l.getTop() - top;
                        final LuaValue[] returnValues = new LuaValue[returnCount];
                        for (int i = 0; i < returnCount; i++) {
                            returnValues[returnCount - i - 1] = l.get();
                        }
                        return returnValues.length == 0 ? null : returnValues;
                    } catch (final LuaException e) {
                        throw new ScriptException(e);
                    }
                }

                @Override
                public ScriptEngine getEngine() {
                    return LuaScriptEngine.this;
                }
            };
        } catch (final LuaException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public CompiledScript compile(final Reader reader) throws ScriptException {
        return compile(readAll(reader));
    }

    private final class LuaScriptEngineBindings implements Bindings {
        @Override
        public int size() {
            return 0;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public boolean containsValue(final Object value) {
            return false;
        }

        @Override
        public void clear() {
            finalizer.clean();
        }

        @Override
        public Set<String> keySet() {
            return Collections.emptySet();
        }

        @Override
        public Collection<Object> values() {
            return Collections.emptyList();
        }

        @Override
        public Set<Entry<String, Object>> entrySet() {
            return Collections.emptySet();
        }

        @Override
        public Object put(final String name, final Object value) {
            final Lua l = getLua();
            putGlobal(l, name, value);
            return null;
        }

        @Override
        public void putAll(final Map<? extends String, ? extends Object> toMerge) {
            final Lua l = getLua();
            for (final Entry<? extends String, ? extends Object> entry : toMerge.entrySet()) {
                putGlobal(l, entry.getKey(), entry.getValue());
            }
        }

        @Override
        public boolean containsKey(final Object key) {
            return get(key) != null;
        }

        @Override
        public Object get(final Object key) {
            try {
                return eval("return " + key);
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public Object remove(final Object key) {
            try {
                run(key + " = nil");
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
            return null;
        }
    }

    private static final class LuaScriptEngineFinalizer extends AFinalizer {

        private Lua lua;

        @Override
        protected void clean() {
            final Lua luaCopy = lua;
            if (luaCopy != null) {
                luaCopy.close();
                lua = null;
            }
        }

        @Override
        protected boolean isCleaned() {
            return lua == null;
        }

        @Override
        public boolean isThreadLocal() {
            return false;
        }

    }

    @Override
    public void close() throws IOException {
        finalizer.close();
    }
}
