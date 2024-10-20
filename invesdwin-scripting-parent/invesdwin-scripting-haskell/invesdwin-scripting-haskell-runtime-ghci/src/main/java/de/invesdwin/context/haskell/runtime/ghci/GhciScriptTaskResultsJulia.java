package de.invesdwin.context.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.TextNode;

import de.invesdwin.context.haskell.runtime.contract.AScriptTaskResultsHaskellFromJson;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Characters;

@NotThreadSafe
public class GhciScriptTaskResultsJulia extends AScriptTaskResultsHaskellFromJson {

    private final GhciScriptTaskEngineJulia engine;

    public GhciScriptTaskResultsJulia(final GhciScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public GhciScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    protected JsonNode getAsJsonNode(final String variable) {
        return engine.unwrap().getAsJsonNode(variable);
    }

    @Override
    public char[] getCharacterVector(final String variable) {
        JsonNode strs = getAsJsonNode(variable);
        if (strs == null) {
            return null;
        }
        if (strs instanceof TextNode) {
            final TextNode cStrs = (TextNode) strs;
            final String text = cStrs.asText();
            final char[] values = new char[text.length()];
            for (int i = 0; i < values.length; i++) {
                values[i] = text.charAt(i);
            }
            return values;
        }
        //unwrap array
        while (strs.size() == 1 && strs.get(0).size() > 1) {
            strs = strs.get(0);
        }
        final char[] values = new char[strs.size()];
        for (int i = 0; i < values.length; i++) {
            final String str = strs.get(i).asText();
            if (Strings.isBlankOrNullText(str)) {
                values[i] = Characters.DEFAULT_MISSING_VALUE;
            } else {
                values[i] = Characters.checkedCast(str);
            }
        }
        return values;
    }

    @Override
    public char[][] getCharacterMatrix(final String variable) {
        final JsonNode strsMatrix = getAsJsonNode(variable);
        if (strsMatrix == null) {
            return null;
        }
        if (strsMatrix.size() == 0) {
            final char[][] emptyMatrix = new char[0][];
            return emptyMatrix;
        }
        final int rows = strsMatrix.size();
        final int columns;
        final JsonNode firstRow = strsMatrix.get(0);
        if (firstRow instanceof TextNode) {
            final TextNode cFirstRow = (TextNode) firstRow;
            columns = cFirstRow.asText().length();
        } else {
            columns = firstRow.size();
        }
        final char[][] valuesMatrix = new char[rows][];
        for (int r = 0; r < rows; r++) {
            final char[] values = new char[columns];
            valuesMatrix[r] = values;
            final JsonNode nodeRow = strsMatrix.get(r);
            if (nodeRow instanceof TextNode) {
                final TextNode cNodeRow = (TextNode) nodeRow;
                final String text = cNodeRow.asText();
                for (int c = 0; c < columns; c++) {
                    values[c] = text.charAt(c);
                }
            } else {
                for (int c = 0; c < columns; c++) {
                    final String str = nodeRow.get(c).asText();
                    if (Strings.isBlankOrNullText(str)) {
                        values[c] = Characters.DEFAULT_MISSING_VALUE;
                    } else {
                        values[c] = Characters.checkedCast(str);
                    }
                }
            }
        }
        return valuesMatrix;
    }

}