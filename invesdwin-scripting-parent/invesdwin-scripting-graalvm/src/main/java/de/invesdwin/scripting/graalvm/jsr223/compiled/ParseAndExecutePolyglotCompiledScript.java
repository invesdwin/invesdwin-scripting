package de.invesdwin.scripting.graalvm.jsr223.compiled;

import javax.annotation.concurrent.Immutable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

import de.invesdwin.scripting.graalvm.jsr223.PolyglotContext;
import de.invesdwin.scripting.graalvm.jsr223.PolyglotScriptEngine;

@Immutable
public class ParseAndExecutePolyglotCompiledScript extends CompiledScript {
    private final ScriptEngine engine;
    private final PolyglotContext context;
    private final Value parsed;

    public ParseAndExecutePolyglotCompiledScript(final Source src, final PolyglotScriptEngine engine)
            throws ScriptException {
        this.engine = engine;
        this.context = engine.getContext();
        try {
            this.parsed = context.getContext().parse(src);
        } catch (final PolyglotException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public Object eval(final ScriptContext context) throws ScriptException {
        if (context != this.context) {
            throw new IllegalArgumentException("context should be defaultContext from engine");
        }
        return parsed.execute().as(Object.class);
    }

    @Override
    public ScriptEngine getEngine() {
        return engine;
    }
}