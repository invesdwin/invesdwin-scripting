package de.invesdwin.scripting.rust.runtime.rhai;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskResultsRustFromJson;

@NotThreadSafe
public class RhaiScriptTaskResultsRust extends AScriptTaskResultsRustFromJson {

    private final RhaiScriptTaskEngineRust engine;

    public RhaiScriptTaskResultsRust(final RhaiScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public RhaiScriptTaskEngineRust getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

}