package de.invesdwin.context.matlab.runtime.javaoctave;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskInputsMatlab;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Doubles;
import dk.ange.octave.type.Octave;
import dk.ange.octave.type.OctaveBoolean;
import dk.ange.octave.type.OctaveCell;
import dk.ange.octave.type.OctaveDouble;
import dk.ange.octave.type.OctaveString;

@NotThreadSafe
public class JavaOctaveScriptTaskInputsMatlab implements IScriptTaskInputsMatlab {

    private final JavaOctaveScriptTaskEngineMatlab engine;

    public JavaOctaveScriptTaskInputsMatlab(final JavaOctaveScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JavaOctaveScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(variable);
        } else {
            engine.unwrap().put(variable, new OctaveString(value));
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final OctaveCell vector = new OctaveCell(value.length, 1);
            for (int i = 0; i < value.length; i++) {
                String str = value[i];
                if (str == null) {
                    str = "";
                }
                vector.set(new OctaveString(str), i + 1, 1);
            }
            engine.unwrap().put(variable, vector);
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final OctaveCell matrix = new OctaveCell(rows, cols);
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    String str = valueRow[col];
                    if (str == null) {
                        str = "";
                    }
                    matrix.set(new OctaveString(str), row + 1, col + 1);
                }
            }
            engine.unwrap().put(variable, matrix);
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        engine.unwrap().put(variable, Octave.scalar(value));
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final OctaveDouble vector = new OctaveDouble(value.length, 1);
            for (int i = 0; i < value.length; i++) {
                vector.set(value[i], i + 1, 1);
            }
            engine.unwrap().put(variable, vector);
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final OctaveDouble matrix = new OctaveDouble(rows, cols);
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    matrix.set(valueRow[col], row + 1, col + 1);
                }
            }
            engine.unwrap().put(variable, matrix);
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        //even though there is the OctaveInt type, it cannot be written...
        putDouble(variable, value);
        putExpression(variable, "int32(" + variable + ")");
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            //even though there is the OctaveInt type, it cannot be written...
            putDoubleVector(variable, Doubles.checkedCastVector(value));
            putExpression(variable, "int32(" + variable + ")");
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            //even though there is the OctaveInt type, it cannot be written...
            putDoubleMatrix(variable, Doubles.checkedCastMatrix(value));
            putExpression(variable, "int32(" + variable + ")");
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        final OctaveBoolean oi = new OctaveBoolean(1, 1);
        oi.set(value, 1, 1);
        engine.unwrap().put(variable, oi);
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final OctaveBoolean vector = new OctaveBoolean(value.length, 1);
            for (int i = 0; i < value.length; i++) {
                vector.set(value[i], i + 1, 1);
            }
            engine.unwrap().put(variable, vector);
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final OctaveBoolean matrix = new OctaveBoolean(rows, cols);
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    matrix.set(valueRow[col], row + 1, col + 1);
                }
            }
            engine.unwrap().put(variable, matrix);
        }
    }

    @Override
    public void putByte(final String variable, final byte value) {
        putDouble(variable, value);
        putExpression(variable, "int8(" + variable + ")");
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[] doubleValue = Doubles.checkedCastVector(value);
            putDoubleVector(variable, doubleValue);
            putExpression(variable, "int8(" + variable + ")");
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int8(" + variable + ")");
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
        putExpression(variable, "single(" + variable + ")");
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
            putExpression(variable, "single(" + variable + ")");
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
            putExpression(variable, "single(" + variable + ")");
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        putDouble(variable, value);
        putExpression(variable, "int16(" + variable + ")");
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[] doubleValue = Doubles.checkedCastVector(value);
            putDoubleVector(variable, doubleValue);
            putExpression(variable, "int16(" + variable + ")");
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[][] doubleValue = Doubles.checkedCastMatrix(value);
            putDoubleMatrix(variable, doubleValue);
            putExpression(variable, "int16(" + variable + ")");
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        putDouble(variable, value);
        putExpression(variable, "int64(" + variable + ")");
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final double[] doubleValue = Doubles.checkedCastVector(value);
            putDoubleVector(variable, doubleValue);
            putExpression(variable, "int64(" + variable + ")");
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
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
