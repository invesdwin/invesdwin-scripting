package de.invesdwin.context.julia.runtime.jajub;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.context.julia.runtime.contract.AScriptTaskResultsJuliaFromJson;

@NotThreadSafe
public class JajubScriptTaskResultsJulia extends AScriptTaskResultsJuliaFromJson {

    private final JajubScriptTaskEngineJulia engine;

    public JajubScriptTaskResultsJulia(final JajubScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public JajubScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }
}