package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.matlab.runtime.contract.AScriptTaskResultsMatlabFromJson;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;

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

    @Override
    protected boolean parseBoolean(final String str) {
        if (Strings.isInteger(str)) {
            return Booleans.checkedCast(Integer.parseInt(str));
        } else {
            return super.parseBoolean(str);
        }
    }

    @Override
    public boolean isDefined(final String variable) {
        return getBoolean("exists('" + variable + "')");
    }

    @Override
    public boolean isNull(final String variable) {
        return getBoolean("~isempty(" + variable + ") & and(isnan(" + variable + "))");
    }

    public boolean isEmpty(final String variable) {
        return getBoolean("isempty(" + variable + ")");
    }

}