package de.invesdwin.context.matlab.runtime.javasci;

import javax.annotation.concurrent.NotThreadSafe;

import org.scilab.modules.javasci.JavasciException;
import org.scilab.modules.javasci.JavasciException.ScilabErrorException;
import org.scilab.modules.types.ScilabBoolean;
import org.scilab.modules.types.ScilabDouble;
import org.scilab.modules.types.ScilabInteger;
import org.scilab.modules.types.ScilabString;
import org.scilab.modules.types.ScilabType;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskInputsMatlab;
import de.invesdwin.util.lang.Objects;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Doubles;

@NotThreadSafe
public class JavasciScriptTaskInputsMatlab implements IScriptTaskInputsMatlab {

    private final JavasciScriptTaskEngineMatlab engine;

    public JavasciScriptTaskInputsMatlab(final JavasciScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JavasciScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(variable);
        } else {
            final ScilabString ss = new ScilabString(value);
            put(variable, ss);
        }
    }

    private void put(final String variable, final ScilabType st) {
        try {
            if (!engine.unwrap().getScilab().put(variable, st)) {
                throw new ScilabErrorException(
                        "A Scilab error occurred: " + engine.unwrap().getScilab().getLastErrorMessage(),
                        engine.unwrap().getScilab().getLastErrorCode());
            }
        } catch (final JavasciException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabString vector = new ScilabString(value);
            replaceNullWithEmpty(vector);
            put(variable, vector);
        }
    }

    private void replaceNullWithEmpty(final ScilabString str) {
        for (final String[] v : str.getData()) {
            for (int i = 0; i < v.length; i++) {
                if (v[i] == null) {
                    v[i] = "";
                }
            }
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else if (value[0].length == 0) {
            //empty string matrix is not really empty in scilab, so we use empty double matrix instead
            putDoubleMatrix(variable, Doubles.checkedCastMatrix(value));
        } else {
            final ScilabString matrix = new ScilabString(Objects.deepClone(value));
            replaceNullWithEmpty(matrix);
            put(variable, matrix);
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        put(variable, new ScilabDouble(value));
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[][] matrix = new double[1][];
            matrix[0] = value;
            final ScilabDouble vector = new ScilabDouble(matrix);
            put(variable, vector);
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabDouble matrix = new ScilabDouble(value);
            put(variable, matrix);
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        put(variable, new ScilabInteger(value));
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final int[][] matrix = new int[1][];
            matrix[0] = value;
            final ScilabInteger vector = new ScilabInteger(matrix, false);
            put(variable, vector);
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabInteger matrix = new ScilabInteger(value, false);
            put(variable, matrix);
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        final ScilabBoolean oi = new ScilabBoolean(value);
        put(variable, oi);
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final boolean[][] matrix = new boolean[1][];
            matrix[0] = value;
            final ScilabBoolean vector = new ScilabBoolean(matrix);
            put(variable, vector);
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabBoolean matrix = new ScilabBoolean(value);
            put(variable, matrix);
        }
    }

    @Override
    public void putByte(final String variable, final byte value) {
        put(variable, new ScilabInteger(value));
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final byte[][] matrix = new byte[1][];
            matrix[0] = value;
            final ScilabInteger vector = new ScilabInteger(matrix, false);
            put(variable, vector);
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabInteger matrix = new ScilabInteger(value, false);
            put(variable, matrix);
        }
    }

    @Override
    public void putCharacter(final String variable, final char value) {
        final String stringValue = Strings.checkedCast(value);
        putString(variable, stringValue);
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
        putDouble(variable, value);
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[] doubleValue = Doubles.checkedCastVector(value);
            putDoubleVector(variable, doubleValue);
        }
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        put(variable, new ScilabInteger(value));
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final short[][] matrix = new short[1][];
            matrix[0] = value;
            final ScilabInteger vector = new ScilabInteger(matrix, false);
            put(variable, vector);
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final ScilabInteger matrix = new ScilabInteger(value, false);
            put(variable, matrix);
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        putDouble(variable, value);
        putExpression(variable, "int64(" + variable + ")");
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        putDoubleVector(variable, Doubles.checkedCastVector(value));
        if (value != null) {
            putExpression(variable, "int64(" + variable + ")");
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        putDoubleMatrix(variable, Doubles.checkedCastMatrix(value));
        if (value != null) {
            putExpression(variable, "int64(" + variable + ")");
        }
    }

    @Override
    public void putNull(final String variable) {
        putExpression(variable, "%nan");
    }

}
