package de.invesdwin.scripting.graalvm.jsr223.compiled;

import javax.annotation.concurrent.Immutable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;

import de.invesdwin.scripting.graalvm.jsr223.PolyglotContext;
import de.invesdwin.scripting.graalvm.jsr223.PolyglotScriptEngine;

@Immutable
public class EvalPolyglotCompiledScript extends CompiledScript {
    private final Source source;
    private final ScriptEngine engine;

    public EvalPolyglotCompiledScript(final Source src, final PolyglotScriptEngine engine) throws ScriptException {
        this.source = src;
        this.engine = engine;
        try {
            engine.getContext().getContext().parse(src); // only for the side-effect of validating the source
        } catch (final PolyglotException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public Object eval(final ScriptContext context) throws ScriptException {
        if (context instanceof PolyglotContext) {
            return ((PolyglotContext) context).getContext().eval(source).as(Object.class);
        }
        throw new UnsupportedOperationException("Polyglot CompiledScript instances can only be evaluated in Polyglot.");
    }

    @Override
    public ScriptEngine getEngine() {
        return engine;
    }
}