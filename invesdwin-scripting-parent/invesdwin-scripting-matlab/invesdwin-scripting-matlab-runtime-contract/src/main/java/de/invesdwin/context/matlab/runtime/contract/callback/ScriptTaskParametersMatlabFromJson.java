package de.invesdwin.context.matlab.runtime.contract.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class ScriptTaskParametersMatlabFromJson extends AScriptTaskParametersMatlabFromJson implements Closeable {

    private JsonNode dims;
    private JsonNode parameters;
    private int offset;

    public void setParameters(final JsonNode dims, final JsonNode parameters, final int offset) {
        this.dims = dims;
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
    protected JsonNode getAsJsonNodeDims(final int index) {
        return dims.get(index + offset);
    }

    @Override
    public void close() {
        dims = null;
        parameters = null;
        offset = 0;
    }

    @Override
    public String toString() {
        return Strings.asString(dims) + "," + Strings.asString(parameters);
    }

}
