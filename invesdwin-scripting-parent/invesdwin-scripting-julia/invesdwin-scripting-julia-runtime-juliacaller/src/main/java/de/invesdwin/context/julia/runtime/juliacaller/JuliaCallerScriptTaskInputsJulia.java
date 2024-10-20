package de.invesdwin.context.julia.runtime.juliacaller;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.julia.runtime.contract.AScriptTaskInputsJuliaToExpression;

@NotThreadSafe
public class JuliaCallerScriptTaskInputsJulia extends AScriptTaskInputsJuliaToExpression {

    private final JuliaCallerScriptTaskEngineJulia engine;

    public JuliaCallerScriptTaskInputsJulia(final JuliaCallerScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public JuliaCallerScriptTaskEngineJulia getEngine() {
        return engine;
    }

}
