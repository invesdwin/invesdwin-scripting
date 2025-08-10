package de.invesdwin.scripting.rust.runtime.evcxr;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskInputsRustToExpression;

@NotThreadSafe
public class EvcxrScriptTaskInputsRust extends AScriptTaskInputsRustToExpression {

    private final EvcxrScriptTaskEngineRust engine;

    public EvcxrScriptTaskInputsRust(final EvcxrScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public void cargoAdd(final String module) {
        engine.eval(":dep " + module);
    }

    @Override
    public EvcxrScriptTaskEngineRust getEngine() {
        return engine;
    }

}
