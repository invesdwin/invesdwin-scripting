package de.invesdwin.scripting.julia.runtime.julia4j;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskInputsJuliaToExpression;

@NotThreadSafe
public class Julia4jScriptTaskInputsJulia extends AScriptTaskInputsJuliaToExpression {

    private final Julia4jScriptTaskEngineJulia engine;

    public Julia4jScriptTaskInputsJulia(final Julia4jScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public Julia4jScriptTaskEngineJulia getEngine() {
        return engine;
    }

}
