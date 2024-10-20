package de.invesdwin.context.matlab.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskInputsMatlabToExpression implements IScriptTaskInputsMatlab {

    private void putEmptyRows(final String variable, final int rows) {
        putExpression(variable, "cell(" + rows + ",1)");
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(value);
        } else {
            putExpression(variable, "'" + value + "'");
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final String v = value[i];
                if (v == null) {
                    sb.append("{''}");
                } else {
                    sb.append("{'");
                    sb.append(v);
                    sb.append("'}");
                }
            }
            sb.append("]");
            putExpression(variable, sb.toString());
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
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final String v = valueRow[col];
                    if (v == null) {
                        sb.append("{''}");
                    } else {
                        sb.append("{'");
                        sb.append(v);
                        sb.append("'}");
                    }
                }
            }
            sb.append("]");
            putExpression(variable, sb.toString());
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final double v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, sb.toString());
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final double v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, sb.toString());
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        putExpression(variable, "int32(" + value + ")");
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final int v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, "int32(" + sb.toString() + ")");
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final int v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, "int32(" + sb.toString() + ")");
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final boolean v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, sb.toString());
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final boolean v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, sb.toString());
        }
    }

    @Override
    public void putByte(final String variable, final byte value) {
        putExpression(variable, "int8(" + value + ")");
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final byte v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, "int8(" + sb.toString() + ")");
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final byte[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final byte v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, "int8(" + sb.toString() + ")");
        }
    }

    @Override
    public void putCharacter(final String variable, final char value) {
        putString(variable, Strings.checkedCast(value));
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
        putExpression(variable, "single(" + value + ")");
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final float v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, "single(" + sb.toString() + ")");
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final float[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final float v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, "single(" + sb.toString() + ")");
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        putExpression(variable, "int16(" + value + ")");
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final short v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, "int16(" + sb.toString() + ")");
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final short[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final short v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, "int16(" + sb.toString() + ")");
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        putExpression(variable, "int64(" + value + ")");
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0) {
            putEmpty(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(" ");
                }
                final long v = value[i];
                sb.append(v);
            }
            sb.append("]");
            putExpression(variable, "int64(" + sb.toString() + ")");
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
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final long[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("; ");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    final long v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("]");
            putExpression(variable, "int64(" + sb.toString() + ")");
        }
    }

    @Override
    public void putNull(final String variable) {
        putExpression(variable, "NaN");
    }

}
