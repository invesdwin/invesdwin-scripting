package de.invesdwin.scripting.python.runtime.jython.pool;

import java.io.Closeable;
import java.io.OutputStreamWriter;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

import org.python.jsr223.PyScriptEngine;
import org.python.jsr223.PyScriptEngineFactory;
import org.zeroturnaround.exec.stream.slf4j.Slf4jDebugOutputStream;
import org.zeroturnaround.exec.stream.slf4j.Slf4jWarnOutputStream;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;

import de.invesdwin.scripting.python.runtime.contract.IScriptTaskRunnerPython;

@NotThreadSafe
public class WrappedPyScriptEngine implements Closeable {

    private static final PyScriptEngineFactory FACTORY = new PyScriptEngineFactory();

    private final LoadingCache<String, CompiledScript> scriptCache;

    private final PyScriptEngine engine;
    private final Compilable compilable;
    private final Invocable invocable;
    private final Bindings binding;
    private final Function<String, Object> evalF;

    public WrappedPyScriptEngine() {
        this.engine = (PyScriptEngine) FACTORY.getScriptEngine();
        engine.getContext().setWriter(new OutputStreamWriter(new Slf4jDebugOutputStream(IScriptTaskRunnerPython.LOG)));
        engine.getContext()
                .setErrorWriter(new OutputStreamWriter(new Slf4jWarnOutputStream(IScriptTaskRunnerPython.LOG)));
        this.binding = engine.getBindings(ScriptContext.ENGINE_SCOPE);
        if (engine instanceof Compilable) {
            compilable = engine;
            scriptCache = Caffeine.newBuilder()
                    .maximumSize(100)
                    .expireAfterAccess(1, TimeUnit.MINUTES)
                    .softValues()
                    .<String, CompiledScript> build((key) -> compilable.compile(key));
            evalF = (expression) -> evalCompiling(expression);
        } else {
            compilable = null;
            scriptCache = null;
            evalF = (expression) -> evalParsing(expression);
        }
        if (engine instanceof Invocable) {
            invocable = engine;
        } else {
            invocable = null;
        }
    }

    public ScriptEngine getEngine() {
        return engine;
    }

    public Bindings getBinding() {
        return binding;
    }

    public Compilable getCompilable() {
        return compilable;
    }

    public Invocable getInvocable() {
        return invocable;
    }

    public Object eval(final String expression) {
        return evalF.apply(expression);
    }

    private Object evalCompiling(final String expression) {
        final CompiledScript parsed = scriptCache.get(expression);
        try {
            return parsed.eval();
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    private Object evalParsing(final String expression) {
        try {
            return engine.eval(expression);
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    public void reset() {
        eval(IScriptTaskRunnerPython.CLEANUP_SCRIPT);
    }

    public void resetScriptCache() {
        if (scriptCache != null) {
            //we have to reset the script cache or ruby throws weird AssertionErrors
            scriptCache.asMap().clear();
        }
    }

    @Override
    public void close() {
        reset();
        engine.close();
    }

    public void put(final String variable, final Object value) {
        binding.put(variable, value);
    }

    public Object get(final String variable) {
        return eval(variable);
    }

    public void remove(final String variable) {
        put(variable, null);
    }

    public boolean contains(final String variable) {
        return binding.get(variable) != null;
    }

}
