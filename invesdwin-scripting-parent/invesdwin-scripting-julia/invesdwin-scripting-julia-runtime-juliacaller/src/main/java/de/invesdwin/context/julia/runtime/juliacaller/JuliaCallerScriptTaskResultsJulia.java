package de.invesdwin.context.julia.runtime.juliacaller;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.context.julia.runtime.contract.AScriptTaskResultsJuliaFromJson;

@NotThreadSafe
public class JuliaCallerScriptTaskResultsJulia extends AScriptTaskResultsJuliaFromJson {

    private final JuliaCallerScriptTaskEngineJulia engine;

    public JuliaCallerScriptTaskResultsJulia(final JuliaCallerScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public JuliaCallerScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        try {
            return engine.unwrap().getAsJsonNode(variable);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

}