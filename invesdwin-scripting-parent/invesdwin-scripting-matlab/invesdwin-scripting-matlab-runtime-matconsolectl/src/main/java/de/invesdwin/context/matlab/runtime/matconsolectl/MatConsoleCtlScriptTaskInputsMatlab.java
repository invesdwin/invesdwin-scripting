package de.invesdwin.context.matlab.runtime.matconsolectl;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskInputsMatlab;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Doubles;
import matlabcontrol.MatlabInvocationException;
import matlabcontrol.extensions.MatlabNumericArray;

@NotThreadSafe
public class MatConsoleCtlScriptTaskInputsMatlab implements IScriptTaskInputsMatlab {

    private final MatConsoleCtlScriptTaskEngineMatlab engine;

    public MatConsoleCtlScriptTaskInputsMatlab(final MatConsoleCtlScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public MatConsoleCtlScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    private void set(final String variable, final Object value) {
        try {
            engine.unwrap().setVariable(variable, value);
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    private void setArray(final String variable, final MatlabNumericArray value) {
        try {
            engine.getTypeConverter().setNumericArray(variable, value);
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    private void putEmptyRows(final String variable, final int rows) {
        putExpression(variable, "cell(" + rows + ",1)");
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final String[] vectorCopy = value.clone();
            for (int i = 0; i < vectorCopy.length; i++) {
                final String str = vectorCopy[i];
                if (str == null) {
                    vectorCopy[i] = "";
                }
            }
            set(variable, vectorCopy);
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final int[] size = new int[] { rows, cols };
            final String[] flatMatrix = new String[rows * cols];
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                for (int col = 0; col < cols; col++) {
                    flatMatrix[engine.pos2ind(size, row, col)] = valueRow[col];
                }
            }
            putStringVector(variable, flatMatrix);
            putExpression(variable, "reshape(" + variable + ", " + rows + ", " + cols + ")");
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        set(variable, value);
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final MatlabNumericArray arr = new MatlabNumericArray(value, null);
            setArray(variable, arr);
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        set(variable, value);
        putExpression(variable, "int32(" + variable + ")");
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int32(" + variable + ")");
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        set(variable, value);
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "logical(" + variable + ")");
        }
    }

    @Override
    public void putByte(final String variable, final byte value) {
        set(variable, value);
        putExpression(variable, "int8(" + variable + ")");
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
            putExpression(variable, "int8(" + variable + ")");
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int8(" + variable + ")");
        }
    }

    @Override
    public void putCharacter(final String variable, final char value) {
        set(variable, value);
    }

    @Override
    public void putCharacterVector(final String variable, final char[] value) {
        final String[] stringValue = Strings.checkedCastVector(value);
        putStringVector(variable, stringValue);
    }

    @Override
    public void putCharacterMatrix(final String variable, final char[][] value) {
        final String[][] stringValue = Strings.checkedCastMatrix(value);
        putStringMatrix(variable, stringValue);
    }

    @Override
    public void putFloat(final String variable, final float value) {
        set(variable, value);
        putExpression(variable, "single(" + variable + ")");
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "single(" + variable + ")");
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        set(variable, value);
        putExpression(variable, "int16(" + variable + ")");
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int16(" + variable + ")");
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        set(variable, value);
        putExpression(variable, "int64(" + variable + ")");
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            set(variable, value);
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            putEmptyRows(variable, value.length);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int64(" + variable + ")");
        }
    }

    @Override
    public void putNull(final String variable) {
        putExpression(variable, "NaN");
    }

}
