package de.invesdwin.context.matlab.runtime.contract.callback;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public abstract class AScriptTaskReturnsMatlabToExpression implements IScriptTaskReturnsMatlab {

    protected void returnEmptyRows(final int rows) {
        returnExpression("cell(" + rows + ",1)");
    }

    @Override
    public void returnString(final String value) {
        if (value == null) {
            returnNull();
        } else {
            returnExpression("'" + value + "'");
        }
    }

    @Override
    public void returnStringVector(final String[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnStringMatrix(final String[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnDouble(final double value) {
        returnExpression(String.valueOf(value));
    }

    @Override
    public void returnDoubleVector(final double[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnDoubleMatrix(final double[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnInteger(final int value) {
        returnExpression("int32(" + value + ")");
    }

    @Override
    public void returnIntegerVector(final int[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression("int32(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnIntegerMatrix(final int[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression("int32(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnBoolean(final boolean value) {
        returnExpression(String.valueOf(value));
    }

    @Override
    public void returnBooleanVector(final boolean[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnBooleanMatrix(final boolean[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnByte(final byte value) {
        returnExpression("int8(" + value + ")");
    }

    @Override
    public void returnByteVector(final byte[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression("int8(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnByteMatrix(final byte[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression("int8(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnCharacter(final char value) {
        returnString(Strings.checkedCast(value));
    }

    @Override
    public void returnCharacterVector(final char[] value) {
        final String[] stringValue = Strings.checkedCastVector(value);
        returnStringVector(stringValue);
    }

    @Override
    public void returnCharacterMatrix(final char[][] value) {
        final String[][] stringValue = Strings.checkedCastMatrix(value);
        returnStringMatrix(stringValue);
    }

    @Override
    public void returnFloat(final float value) {
        returnExpression("single(" + value + ")");
    }

    @Override
    public void returnFloatVector(final float[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression("single(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnFloatMatrix(final float[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression("single(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnShort(final short value) {
        returnExpression("int16(" + value + ")");
    }

    @Override
    public void returnShortVector(final short[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression("int16(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnShortMatrix(final short[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression("int16(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnLong(final long value) {
        returnExpression("int64(" + value + ")");
    }

    @Override
    public void returnLongVector(final long[] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
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
            returnExpression("int64(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnLongMatrix(final long[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0) {
            returnEmpty();
        } else if (value[0].length == 0) {
            returnEmptyRows(value.length);
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
            returnExpression("int64(" + sb.toString() + ")");
        }
    }

    @Override
    public void returnNull() {
        returnExpression("NaN");
    }

}
