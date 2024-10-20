package de.invesdwin.context.r.runtime.contract.callback;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.script.callback.AScriptTaskParametersFromString;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskParametersRFromJson extends AScriptTaskParametersFromString
        implements IScriptTaskParametersR {

    protected abstract JsonNode getAsJsonNode(int index);

    protected abstract JsonNode getAsJsonNodeDims(int index);

    @Override
    public boolean isNull(final int index) {
        JsonNode node = getAsJsonNode(index);
        if (node == null) {
            return true;
        }
        while (node.size() == 1) {
            node = node.get(0);
        }
        return node instanceof NullNode;
    }

    @Override
    public boolean isEmpty(final int index) {
        final JsonNode node = getAsJsonNode(index);
        if (node == null) {
            return true;
        }
        return node.isEmpty();
    }

    @Override
    public String getString(final int index) {
        JsonNode node = getAsJsonNode(index);
        if (node == null) {
            return null;
        }
        while (node.size() == 1) {
            node = node.get(0);
        }
        final String str = node.asText();
        if (Strings.isBlankOrNullText(str)) {
            return null;
        } else {
            return str;
        }
    }

    @Override
    public String[] getStringVector(final int index) {
        JsonNode strs = getAsJsonNode(index);
        if (strs == null) {
            return null;
        }
        //unwrap array
        while (strs.size() == 1 && strs.get(0).size() > 1) {
            strs = strs.get(0);
        }
        final String[] values = new String[strs.size()];
        for (int i = 0; i < values.length; i++) {
            final String str = strs.get(i).asText();
            if (Strings.isBlankOrNullText(str)) {
                values[i] = null;
            } else {
                values[i] = str;
            }
        }
        return values;
    }

    @Override
    public String[][] getStringMatrix(final int index) {
        final JsonNode strsMatrix = getAsJsonNode(index);
        if (strsMatrix == null) {
            return null;
        }
        if (strsMatrix.size() == 0) {
            final JsonNode dims = getAsJsonNodeDims(index);
            final int rows = dims.get(0).asInt();
            final String[][] emptyMatrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                emptyMatrix[i] = Strings.EMPTY_ARRAY;
            }
            return emptyMatrix;
        }
        final int rows = strsMatrix.size();
        final int columns = strsMatrix.get(0).size();
        final String[][] valuesMatrix = new String[rows][];
        for (int r = 0; r < rows; r++) {
            final String[] values = new String[columns];
            valuesMatrix[r] = values;
            final JsonNode nodeRow = strsMatrix.get(r);
            for (int c = 0; c < columns; c++) {
                final String str = nodeRow.get(c).asText();
                if (Strings.isBlankOrNullText(str)) {
                    values[c] = null;
                } else {
                    values[c] = str;
                }
            }
        }
        return valuesMatrix;
    }

}