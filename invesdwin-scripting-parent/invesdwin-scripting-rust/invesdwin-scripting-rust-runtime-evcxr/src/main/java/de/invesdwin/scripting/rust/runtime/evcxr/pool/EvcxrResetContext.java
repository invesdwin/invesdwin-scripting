package de.invesdwin.scripting.rust.runtime.evcxr.pool;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;

@NotThreadSafe
public class EvcxrResetContext {

    private final IScriptTaskEngine engine;

    public EvcxrResetContext(final IScriptTaskEngine engine) {
        this.engine = engine;
    }

    public void init() {
        engine.eval(new ClassPathResource(EvcxrResetContext.class.getSimpleName() + ".rs", EvcxrResetContext.class));
    }

    public void reset() {
        engine.eval("restoreContext()");
    }

}
