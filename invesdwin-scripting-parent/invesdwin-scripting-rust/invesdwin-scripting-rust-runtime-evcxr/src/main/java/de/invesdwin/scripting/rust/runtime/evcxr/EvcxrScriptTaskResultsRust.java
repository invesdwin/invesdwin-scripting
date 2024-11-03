package de.invesdwin.scripting.rust.runtime.evcxr;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskResultsRustFromJson;

@NotThreadSafe
public class EvcxrScriptTaskResultsRust extends AScriptTaskResultsRustFromJson {

    private final EvcxrScriptTaskEngineRust engine;

    public EvcxrScriptTaskResultsRust(final EvcxrScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public EvcxrScriptTaskEngineRust getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

}