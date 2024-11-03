package de.invesdwin.scripting.rust.runtime.rhai.pool;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;

@NotThreadSafe
public class RhaiResetContext {

    private final IScriptTaskEngine engine;

    public RhaiResetContext(final IScriptTaskEngine engine) {
        this.engine = engine;
    }

    public void init() {
        engine.eval(new ClassPathResource(RhaiResetContext.class.getSimpleName() + ".rs", RhaiResetContext.class));
    }

    public void reset() {
        engine.eval("restoreContext()");
    }

}
