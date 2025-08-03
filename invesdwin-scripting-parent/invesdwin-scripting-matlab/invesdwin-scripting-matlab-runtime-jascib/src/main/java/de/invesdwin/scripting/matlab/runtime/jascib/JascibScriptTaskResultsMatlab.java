package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.matlab.runtime.contract.AScriptTaskResultsMatlabFromJson;

@NotThreadSafe
public class JascibScriptTaskResultsMatlab extends AScriptTaskResultsMatlabFromJson {

    private final JascibScriptTaskEngineMatlab engine;

    public JascibScriptTaskResultsMatlab(final JascibScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JascibScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

    @Override
    protected JsonNode getAsJsonNodeDims(final String variable) {
        return engine.unwrap().getAsJsonNode("size(" + variable + ")");
    }

}