package de.invesdwin.scripting.rust.runtime.rhai;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskInputsRustToExpression;

@NotThreadSafe
public class RhaiScriptTaskInputsRust extends AScriptTaskInputsRustToExpression {

    private final RhaiScriptTaskEngineRust engine;

    public RhaiScriptTaskInputsRust(final RhaiScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public RhaiScriptTaskEngineRust getEngine() {
        return engine;
    }

}
