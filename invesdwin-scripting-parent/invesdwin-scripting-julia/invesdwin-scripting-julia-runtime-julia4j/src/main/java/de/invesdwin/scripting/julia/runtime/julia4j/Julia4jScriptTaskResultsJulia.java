package de.invesdwin.scripting.julia.runtime.julia4j;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskResultsJuliaFromJson;

@NotThreadSafe
public class Julia4jScriptTaskResultsJulia extends AScriptTaskResultsJuliaFromJson {

    private final Julia4jScriptTaskEngineJulia engine;

    public Julia4jScriptTaskResultsJulia(final Julia4jScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public Julia4jScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

}