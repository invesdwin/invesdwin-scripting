package de.invesdwin.context.matlab.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.script.AScriptTaskResultsFromString;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskResultsMatlabFromJson extends AScriptTaskResultsFromString
        implements IScriptTaskResultsMatlab {

    protected abstract JsonNode getAsJsonNode(String variable);

    protected abstract JsonNode getAsJsonNodeDims(String variable);

    @Override
    public boolean isNull(final String variable) {
        JsonNode node = getAsJsonNode(variable);
        if (node == null) {
            return true;
        }
        while (node.size() == 1) {
            node = node.get(0);
        }
        return node instanceof NullNode;
    }

    @Override
    public String getString(final String variable) {
        final JsonNode node = getAsJsonNode(variable);
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
    public String[] getStringVector(final String variable) {
        JsonNode strs = getAsJsonNode(variable);
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
    public String[][] getStringMatrix(final String variable) {
        final JsonNode strsMatrix = getAsJsonNode(variable);
        if (strsMatrix == null) {
            return null;
        }
        if (strsMatrix.size() == 0) {
            final JsonNode dims = getAsJsonNodeDims(variable);
            final int rows = dims.get(0).asInt();
            final String[][] emptyMatrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                emptyMatrix[i] = Strings.EMPTY_ARRAY;
            }
            return emptyMatrix;
        }
        final int jsonRows = strsMatrix.size();
        final int jsonColumns = strsMatrix.get(0).size();
        if (jsonColumns == 0 && !(strsMatrix.get(0) instanceof ArrayNode)) {
            //            >> a=[{'1-1'} {'1-2'} {'1-3'}; {'2-1'} {'2-2'} {'2-3'}; {'3-1'} {'3-2'} {'3-3'}; {'4-1'} {'4-2'} {'4-3'}]
            //              4×3 cell array
            //                {'1-1'}    {'1-2'}    {'1-3'}
            //                {'2-1'}    {'2-2'}    {'2-3'}
            //                {'3-1'}    {'3-2'}    {'3-3'}
            //                {'4-1'}    {'4-2'}    {'4-3'}
            //            >> jsonencode(a)
            //                '["1-1","2-1","3-1","4-1","1-2","2-2","3-2","4-2","1-3","2-3","3-3","4-3"]'
            final JsonNode dims = getAsJsonNodeDims(variable);
            final int rows = dims.get(0).asInt();
            final int columns = dims.get(1).asInt();
            final String[][] valuesMatrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                valuesMatrix[i] = new String[columns];
            }
            int jsonIdx = 0;
            for (int c = 0; c < columns; c++) {
                for (int r = 0; r < rows; r++) {
                    final String str = strsMatrix.get(jsonIdx).asText();
                    if (Strings.isBlankOrNullText(str)) {
                        valuesMatrix[r][c] = null;
                    } else {
                        valuesMatrix[r][c] = str;
                    }
                    jsonIdx++;
                }
            }
            assert jsonIdx == jsonRows;
            return valuesMatrix;
        } else {
            final int rows = jsonRows;
            final int columns = jsonColumns;
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

}