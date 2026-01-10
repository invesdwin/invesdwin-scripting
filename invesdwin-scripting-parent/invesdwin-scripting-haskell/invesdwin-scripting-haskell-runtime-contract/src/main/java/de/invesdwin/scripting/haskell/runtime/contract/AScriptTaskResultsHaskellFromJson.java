package de.invesdwin.scripting.haskell.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.TextNode;

import de.invesdwin.scripting.AScriptTaskResultsFromString;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskResultsHaskellFromJson extends AScriptTaskResultsFromString
        implements IScriptTaskResultsHaskell {

    protected abstract JsonNode getAsJsonNode(String variable);

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
        if (strs.size() == 0 && strs instanceof TextNode) {
            final String text = strs.asText();
            final String[] characters = new String[text.length()];
            for (int i = 0; i < characters.length; i++) {
                characters[i] = String.valueOf(text.charAt(i));
            }
            return characters;
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
            final String[][] emptyMatrix = new String[0][];
            return emptyMatrix;
        }
        final int rows = strsMatrix.size();
        final int columns;
        final JsonNode sampleRow = strsMatrix.get(0);
        if (sampleRow.size() == 0 && sampleRow instanceof TextNode) {
            columns = sampleRow.asText().length();
        } else {
            columns = sampleRow.size();
        }

        final String[][] valuesMatrix = new String[rows][];
        for (int r = 0; r < rows; r++) {
            final String[] values = new String[columns];
            valuesMatrix[r] = values;
            final JsonNode nodeRow = strsMatrix.get(r);
            if (nodeRow.size() == 0 && nodeRow instanceof TextNode) {
                final String text = nodeRow.asText();
                for (int c = 0; c < columns; c++) {
                    values[c] = String.valueOf(text.charAt(c));
                }
            } else {
                for (int c = 0; c < columns; c++) {
                    final String str = nodeRow.get(c).asText();
                    if (Strings.isBlankOrNullText(str)) {
                        values[c] = null;
                    } else {
                        values[c] = str;
                    }
                }
            }
        }
        return valuesMatrix;
    }

}