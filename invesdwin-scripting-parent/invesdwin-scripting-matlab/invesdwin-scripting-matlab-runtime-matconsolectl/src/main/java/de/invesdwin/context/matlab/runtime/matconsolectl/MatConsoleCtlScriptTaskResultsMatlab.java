package de.invesdwin.context.matlab.runtime.matconsolectl;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskResultsMatlab;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Doubles;
import de.invesdwin.util.math.Floats;
import de.invesdwin.util.math.Integers;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;
import matlabcontrol.MatlabInvocationException;
import matlabcontrol.extensions.MatlabNumericArray;

@NotThreadSafe
public class MatConsoleCtlScriptTaskResultsMatlab implements IScriptTaskResultsMatlab {

    private final MatConsoleCtlScriptTaskEngineMatlab engine;

    public MatConsoleCtlScriptTaskResultsMatlab(final MatConsoleCtlScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public MatConsoleCtlScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    private Object get(final String variable) {
        try {
            return engine.unwrap().returningEval(variable, 1)[0];
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    private MatlabNumericArray getArray(final String variable) {
        try {
            return engine.getTypeConverter().getNumericArray(variable);
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String getString(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final Object obj = get(variable);
            return Strings.checkedCast(obj);
        }
    }

    @Override
    public String[] getStringVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new String[0];
        } else {
            final Object obj = get(variable);
            final String[] vector = Strings.checkedCastVector(obj);
            for (int i = 0; i < vector.length; i++) {
                final String str = vector[i];
                if (Strings.isEmpty(str)) {
                    vector[i] = null;
                }
            }
            return vector;
        }
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final String[][] matrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new String[0];
            }
            return matrix;
        } else {
            final int[] size = getIntegerVector("size(" + variable + ")");
            Assertions.checkEquals(2, size.length, "Matrix expected");
            final int rows = size[0];
            final int cols = size[1];
            final String[] flatMatrix = getStringVector(variable);
            final String[][] matrix = new String[rows][];
            for (int row = 0; row < rows; row++) {
                final String[] vector = new String[cols];
                for (int col = 0; col < cols; col++) {
                    final int ind = engine.pos2ind(size, row, col);
                    vector[col] = flatMatrix[ind];
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    @Override
    public double getDouble(final String variable) {
        final Object obj = get(variable);
        return Doubles.checkedCast(obj);
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new double[0];
        } else {
            final Object obj = get(variable);
            return Doubles.checkedCastVector(obj);
        }
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final double[][] matrix = new double[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new double[0];
            }
            return matrix;
        } else {
            final MatlabNumericArray obj = getArray("double(" + variable + ")");
            final double[][] value = obj.getRealArray2D();
            return Doubles.checkedCastMatrix(value);
        }
    }

    @Override
    public int getInteger(final String variable) {
        final Object obj = get(variable);
        return Integers.checkedCast(obj);
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new int[0];
        } else {
            final Object obj = get(variable);
            return Integers.checkedCastVector(obj);
        }
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        return Integers.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public boolean getBoolean(final String variable) {
        final Object obj = get(variable);
        return Booleans.checkedCast(obj);
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new boolean[0];
        } else {
            final Object obj = get(variable);
            return Booleans.checkedCastVector(obj);
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        return Booleans.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public byte getByte(final String variable) {
        final Object obj = get(variable);
        return Bytes.checkedCast(obj);
    }

    @Override
    public byte[] getByteVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new byte[0];
        } else {
            final Object obj = get(variable);
            return Bytes.checkedCastVector(obj);
        }
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        return Bytes.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public char getCharacter(final String variable) {
        final Object obj = get(variable);
        return Characters.checkedCast(obj);
    }

    @Override
    public char[] getCharacterVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new char[0];
        } else {
            final Object obj = get(variable);
            return Characters.checkedCastVector(obj);
        }
    }

    @Override
    public char[][] getCharacterMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final char[][] matrix = new char[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new char[0];
            }
            return matrix;
        } else {
            final String[][] obj = getStringMatrix(variable);
            return Characters.checkedCastMatrix(obj);
        }
    }

    @Override
    public float getFloat(final String variable) {
        final Object obj = get(variable);
        return Floats.checkedCast(obj);
    }

    @Override
    public float[] getFloatVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new float[0];
        } else {
            final Object obj = get(variable);
            return Floats.checkedCastVector(obj);
        }
    }

    @Override
    public float[][] getFloatMatrix(final String variable) {
        return Floats.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public short getShort(final String variable) {
        final Object obj = get(variable);
        return Shorts.checkedCast(obj);
    }

    @Override
    public short[] getShortVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new short[0];
        } else {
            final Object obj = get(variable);
            return Shorts.checkedCastVector(obj);
        }
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        return Shorts.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public long getLong(final String variable) {
        final Object obj = get(variable);
        return Longs.checkedCast(obj);
    }

    @Override
    public long[] getLongVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new long[0];
        } else {
            final Object obj = get(variable);
            return Longs.checkedCastVector(obj);
        }
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        return Longs.checkedCastMatrix(getDoubleMatrix(variable));
    }

    @Override
    public boolean isNull(final String variable) {
        return getBoolean(
                "isnumeric(" + variable + ") && length(" + variable + ") == 1 && ismissing(" + variable + ")");
    }

    public boolean isEmpty(final String variable) {
        if (getBoolean("iscell(" + variable + ")")) {
            getEngine().getInputs().putExpression(MatConsoleCtlScriptTaskRunnerMatlab.INTERNAL_RESULT_VARIABLE,
                    "cellfun(@isempty, " + variable + ")");
            return getBoolean("all(" + MatConsoleCtlScriptTaskRunnerMatlab.INTERNAL_RESULT_VARIABLE + "(:))");
        } else {
            return getBoolean("isempty(" + variable + ")");
        }
    }

}