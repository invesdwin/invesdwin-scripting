package de.invesdwin.scripting.haskell.runtime.frege.callback.file;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

// @NotThreadSafe
public class FregeEval {

    private final ScriptEngine engine;

    public FregeEval() {
        final ScriptEngineManager manager = new ScriptEngineManager();
        this.engine = manager.getEngineByName("frege");
    }

    public ScriptEngine getEngine() {
        return engine;
    }

    @SuppressWarnings("unchecked")
    public <T> T eval(final String expression) {
        try {
            final Object result = engine.eval(expression);
            return (T) result;
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

}
