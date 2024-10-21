package de.invesdwin.scripting.julia.runtime.juliacaller;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskInputsJuliaToExpression;

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
