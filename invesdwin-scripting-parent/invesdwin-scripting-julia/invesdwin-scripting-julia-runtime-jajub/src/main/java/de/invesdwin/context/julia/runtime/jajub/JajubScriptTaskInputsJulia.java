package de.invesdwin.context.julia.runtime.jajub;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.julia.runtime.contract.AScriptTaskInputsJuliaToExpression;

@NotThreadSafe
public class JajubScriptTaskInputsJulia extends AScriptTaskInputsJuliaToExpression {

    private final JajubScriptTaskEngineJulia engine;

    public JajubScriptTaskInputsJulia(final JajubScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public JajubScriptTaskEngineJulia getEngine() {
        return engine;
    }

}
