package de.invesdwin.scripting.haskell.runtime.contract.callback;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.scripting.callback.AScriptTaskParametersFromString;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskParametersHaskellFromJson extends AScriptTaskParametersFromString {

    protected abstract JsonNode getAsJsonNode(int index);

    @Override
    public boolean isNull(final int index) {
        final JsonNode node = getAsJsonNode(index);
        return node == null || node instanceof NullNode;
    }

    @Override
    public String getString(final int index) {
        final JsonNode node = getAsJsonNode(index);
        if (node == null) {
            return null;
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
        //json returns the columns instead of rows
        final JsonNode strsMatrix = getAsJsonNode(index);
        if (strsMatrix == null) {
            return null;
        }
        //[11 12 13;21 22 23;31 32 33;41 42 43]
        //[[11,21,31,41],[12,22,32,42],[13,23,33,43]]
        final int columns = strsMatrix.size();
        final int rows = strsMatrix.get(0).size();
        final String[][] valuesMatrix = new String[rows][];
        for (int r = 0; r < rows; r++) {
            final String[] values = new String[columns];
            valuesMatrix[r] = values;
            for (int c = 0; c < columns; c++) {
                final String str = strsMatrix.get(c).get(r).asText();
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