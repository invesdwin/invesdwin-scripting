package de.invesdwin.scripting.haskell.runtime.frege;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskResultsHaskellFromJson;

@NotThreadSafe
public class FregeScriptTaskResultsHaskell extends AScriptTaskResultsHaskellFromJson {

    private final FregeScriptTaskEngineHaskell engine;

    public FregeScriptTaskResultsHaskell(final FregeScriptTaskEngineHaskell engine) {
        this.engine = engine;
    }

    @Override
    public FregeScriptTaskEngineHaskell getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

}