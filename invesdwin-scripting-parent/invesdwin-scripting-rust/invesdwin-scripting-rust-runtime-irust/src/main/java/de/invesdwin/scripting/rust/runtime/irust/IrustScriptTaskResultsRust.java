package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskResultsRustFromJson;

@NotThreadSafe
public class IrustScriptTaskResultsRust extends AScriptTaskResultsRustFromJson {

    private final IrustScriptTaskEngineRust engine;

    public IrustScriptTaskResultsRust(final IrustScriptTaskEngineRust engine) {
        this.engine = engine;
    }

    @Override
    public IrustScriptTaskEngineRust getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

}