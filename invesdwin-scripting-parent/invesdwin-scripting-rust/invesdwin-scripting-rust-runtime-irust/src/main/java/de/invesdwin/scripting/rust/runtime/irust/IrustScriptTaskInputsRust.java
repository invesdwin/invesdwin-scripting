package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskInputsRustToExpression;

@NotThreadSafe
public class IrustScriptTaskInputsRust extends AScriptTaskInputsRustToExpression {

    private final IrustScriptTaskEngineRust engine;

    public IrustScriptTaskInputsRust(final IrustScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public void cargoAdd(final String module) {
        engine.eval(":add " + module);
    }

    @Override
    public IrustScriptTaskEngineRust getEngine() {
        return engine;
    }

}
