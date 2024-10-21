package de.invesdwin.scripting.julia.runtime.libjuliaclj;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskResultsJulia;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Integers;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;

@NotThreadSafe
public class LibjuliacljScriptTaskResultsJulia implements IScriptTaskResultsJulia {

    private final LibjuliacljScriptTaskEngineJulia engine;

    public LibjuliacljScriptTaskResultsJulia(final LibjuliacljScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public LibjuliacljScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    public String getString(final String variable) {
        final JsonNode node = engine.unwrap().getAsJsonNode(variable);
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
        return getEngine().unwrap().getStringVectorAsJson(variable);
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        return getEngine().unwrap().getStringMatrixAsJson(variable);
    }

    @Override
    public char getCharacter(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Characters.DEFAULT_MISSING_VALUE;
        } else {
            return Characters.checkedCast(str);
        }
    }

    @Override
    public char[] getCharacterVector(final String variable) {
        return getEngine().unwrap().getCharacterVectorAsJson(variable);
    }

    @Override
    public char[][] getCharacterMatrix(final String variable) {
        return getEngine().unwrap().getCharacterMatrixAsJson(variable);
    }

    @Override
    public boolean getBoolean(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Booleans.DEFAULT_MISSING_VALUE;
        } else {
            return Boolean.parseBoolean(str);
        }
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        return getEngine().unwrap().getBooleanVectorAsJson(variable);
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        return getEngine().unwrap().getBooleanMatrixAsJson(variable);
    }

    @Override
    public byte getByte(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Bytes.DEFAULT_MISSING_VALUE;
        } else {
            return Byte.parseByte(str);
        }
    }

    @Override
    public byte[] getByteVector(final String variable) {
        return getEngine().unwrap().getByteVector(variable);
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        return getEngine().unwrap().getByteMatrix(variable);
    }

    @Override
    public short getShort(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Shorts.DEFAULT_MISSING_VALUE;
        } else {
            return Short.parseShort(str);
        }
    }

    @Override
    public short[] getShortVector(final String variable) {
        return getEngine().unwrap().getShortVector(variable);
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        return getEngine().unwrap().getShortMatrix(variable);
    }

    @Override
    public int getInteger(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Integers.DEFAULT_MISSING_VALUE;
        } else {
            return Integer.parseInt(str);
        }
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        return getEngine().unwrap().getIntegerVector(variable);
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        return getEngine().unwrap().getIntegerMatrix(variable);
    }

    @Override
    public long getLong(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Longs.DEFAULT_MISSING_VALUE;
        } else {
            return Long.parseLong(str);
        }
    }

    @Override
    public long[] getLongVector(final String variable) {
        return getEngine().unwrap().getLongVector(variable);
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        return getEngine().unwrap().getLongMatrix(variable);
    }

    @Override
    public float getFloat(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Float.NaN;
        } else {
            return Float.parseFloat(str);
        }
    }

    @Override
    public float[] getFloatVector(final String variable) {
        return getEngine().unwrap().getFloatVector(variable);
    }

    @Override
    public float[][] getFloatMatrix(final String variable) {
        return getEngine().unwrap().getFloatMatrix(variable);
    }

    @Override
    public double getDouble(final String variable) {
        final String str = getString(variable);
        if (str == null) {
            return Double.NaN;
        } else {
            return Double.parseDouble(str);
        }
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        return getEngine().unwrap().getDoubleVector(variable);
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        return getEngine().unwrap().getDoubleMatrix(variable);
    }

}