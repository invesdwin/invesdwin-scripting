package de.invesdwin.scripting.haskell.runtime.contract.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class ScriptTaskParametersHaskellFromJson extends AScriptTaskParametersHaskellFromJson implements Closeable {

    private JsonNode parameters;
    private int offset;

    public void setParameters(final JsonNode parameters, final int offset) {
        this.parameters = parameters;
        this.offset = offset;
    }

    @Override
    public int size() {
        return parameters.size() - offset;
    }

    @Override
    protected JsonNode getAsJsonNode(final int index) {
        return parameters.get(index + offset);
    }

    @Override
    public void close() {
        parameters = null;
        offset = 0;
    }

    @Override
    public String toString() {
        return Strings.asString(parameters);
    }

}
