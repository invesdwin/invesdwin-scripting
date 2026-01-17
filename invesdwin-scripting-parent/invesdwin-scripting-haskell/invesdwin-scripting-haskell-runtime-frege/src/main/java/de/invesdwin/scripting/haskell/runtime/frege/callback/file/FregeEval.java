package de.invesdwin.scripting.haskell.runtime.frege.callback.file;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import de.invesdwin.util.error.Throwables;
import frege.prelude.PreludeBase.WrappedCheckedException;

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
        } catch (final WrappedCheckedException e) {
            final NoSuchFieldException noSuchFieldException = Throwables.getCauseByType(e, NoSuchFieldException.class);
            if (noSuchFieldException != null) {
                if ("Unit".equals(noSuchFieldException.getMessage())) {
                    return null;
                } else {
                    throw e;
                }
            }
            final NullPointerException nullPointerException = Throwables.getCauseByType(e, NullPointerException.class);
            if (nullPointerException != null) {
                if (nullPointerException.getMessage()
                        .equals("Cannot invoke \"java.lang.Short.shortValue()\" because the return value of \"frege.run8.Lazy.call()\" is null")) {
                    return null;
                } else {
                    throw e;
                }
            }
            throw e;
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

}
