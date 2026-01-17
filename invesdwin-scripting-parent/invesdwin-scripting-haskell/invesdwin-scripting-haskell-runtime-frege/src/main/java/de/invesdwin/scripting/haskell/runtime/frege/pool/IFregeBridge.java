package de.invesdwin.scripting.haskell.runtime.frege.pool;

import com.fasterxml.jackson.databind.JsonNode;

public interface IFregeBridge {

    void eval(String expression);

    JsonNode getAsJsonNode(String variable);

    void load(String filename, String content);

}
