package de.invesdwin.scripting.python.runtime.japyb.pool;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;

@NotThreadSafe
public class PythonResetContext {

    private final IScriptTaskEngine engine;

    public PythonResetContext(final IScriptTaskEngine engine) {
        this.engine = engine;
    }

    public void init() {
        engine.eval(new ClassPathResource(PythonResetContext.class.getSimpleName() + ".py", PythonResetContext.class));
    }

    public void reset() {
        engine.eval("restoreContext()");
    }

}
