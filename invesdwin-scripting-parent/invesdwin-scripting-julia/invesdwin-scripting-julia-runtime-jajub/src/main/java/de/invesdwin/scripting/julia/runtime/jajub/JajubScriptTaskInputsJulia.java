package de.invesdwin.scripting.julia.runtime.jajub;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskInputsJuliaToExpression;

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
