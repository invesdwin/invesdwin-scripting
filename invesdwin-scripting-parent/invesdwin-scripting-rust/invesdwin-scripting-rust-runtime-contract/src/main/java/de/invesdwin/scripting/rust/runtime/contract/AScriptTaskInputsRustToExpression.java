package de.invesdwin.scripting.rust.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.math.Doubles;

@NotThreadSafe
public abstract class AScriptTaskInputsRustToExpression implements IScriptTaskInputsRust {

    @Override
    public void putCharacter(final String variable, final char value) {
        putExpression(variable, "'" + value + "'");
    }

    @Override
    public void putCharacterVector(final String variable, final char[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append("'");
                sb.append(value[i]);
                sb.append("'");
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_CHARACTER, value.length), sb.toString());
        }
    }

    @Override
    public void putCharacterMatrix(final String variable, final char[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_CHARACTER, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final char[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append("'");
                    sb.append(valueRow[col]);
                    sb.append("'");
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_CHARACTER, cols, rows), sb.toString());
        }
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(variable);
        } else {
            putExpression(variable, "\"" + value + "\"");
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final String v = value[i];
                if (v == null) {
                    sb.append("\"\"");
                } else {
                    sb.append("\"");
                    sb.append(v);
                    sb.append("\"");
                }
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_STRING, value.length), sb.toString());
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_STRING, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final String v = valueRow[col];
                    if (v == null) {
                        sb.append("\"\"");
                    } else {
                        sb.append("\"");
                        sb.append(v);
                        sb.append("\"");
                    }
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_STRING, cols, rows), sb.toString());
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        putExpression(variable, booleanToString(value));
    }

    private String booleanToString(final boolean value) {
        if (value) {
            return "true";
        } else {
            return "false";
        }
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(booleanToString(value[i]));
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_BOOLEAN, value.length), sb.toString());
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_BOOLEAN, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(booleanToString(valueRow[col]));
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_BOOLEAN, cols, rows), sb.toString());
        }
    }

    public void putEmptyMatrix(final String variable, final String type, final int rows) {
        final StringBuilder sb = new StringBuilder("[");
        for (int row = 0; row < rows; row++) {
            if (row > 0) {
                sb.append(",");
            }
            sb.append("[]");
        }
        sb.append("]");
        putExpression(newMatrixVariable(variable, type, 0, rows), sb.toString());
    }

    private String newMatrixVariable(final String variable, final String type, final int cols, final int rows) {
        return variable + ": [[" + type + "; " + cols + "]; " + rows + "]";
    }

    private String newVectorVariable(final String variable, final String type, final int cols) {
        return variable + ": [" + type + "; " + cols + "]";
    }

    @Override
    public void putByte(final String variable, final byte value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_BYTE, value.length), sb.toString());
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_BYTE, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final byte[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(valueRow[col]);
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_BYTE, cols, rows), sb.toString());
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_SHORT, value.length), sb.toString());
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_SHORT, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final short[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(valueRow[col]);
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_SHORT, cols, rows), sb.toString());
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_INTEGER, value.length), sb.toString());
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_INTEGER, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(valueRow[col]);
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_INTEGER, cols, rows), sb.toString());
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_LONG, value.length), sb.toString());
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_LONG, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final long[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(valueRow[col]);
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_LONG, cols, rows), sb.toString());
        }
    }

    @Override
    public void putFloat(final String variable, final float value) {
        putExpression(variable, String.valueOf(value));
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_FLOAT, value.length), sb.toString());
        }
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_FLOAT, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final float[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    sb.append(valueRow[col]);
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_FLOAT, cols, rows), sb.toString());
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        putExpression(variable, doubleToString(value));
    }

    private String doubleToString(final double value) {
        if (Doubles.isNaN(value)) {
            return NAN;
        } else {
            return String.valueOf(value);
        }
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final double v = value[i];
                sb.append(doubleToString(v));
            }
            sb.append("]");
            putExpression(newVectorVariable(variable, TYPE_DOUBLE, value.length), sb.toString());
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putEmptyMatrix(variable, TYPE_DOUBLE, value.length);
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("[");
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(",");
                }
                sb.append("[");
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final double v = valueRow[col];
                    sb.append(doubleToString(v));
                }
                sb.append("]");
            }
            sb.append("]");
            putExpression(newMatrixVariable(variable, TYPE_DOUBLE, cols, rows), sb.toString());
        }
    }

}
