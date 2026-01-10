package de.invesdwin.scripting.haskell.runtime.contract.callback;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.callback.IScriptTaskReturns;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public abstract class AScriptTaskReturnsHaskellToExpression implements IScriptTaskReturns {

    @Override
    public void returnCharacter(final char value) {
        returnExpression("Char('" + value + "')");
    }

    @Override
    public void returnCharacterVector(final char[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Char}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append("'");
                sb.append(value[i]);
                sb.append("'");
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnCharacterMatrix(final char[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Char}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Char}([");
            for (int row = 0; row < rows; row++) {
                final char[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append("'");
                    sb.append(valueRow[col]);
                    sb.append("'");
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnString(final String value) {
        if (value == null) {
            returnNull();
        } else {
            returnExpression("\"" + value + "\"");
        }
    }

    @Override
    public void returnStringVector(final String[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{String}([");
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
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnStringMatrix(final String[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{String}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{String}([");
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
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
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnBoolean(final boolean value) {
        returnExpression("Bool(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnBooleanVector(final boolean[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Bool}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnBooleanMatrix(final boolean[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Bool}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Bool}([");
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnByte(final byte value) {
        returnExpression("Int8(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnByteVector(final byte[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Int8}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnByteMatrix(final byte[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Int8}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Int8}([");
            for (int row = 0; row < rows; row++) {
                final byte[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnShort(final short value) {
        returnExpression("Int16(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnShortVector(final short[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Int16}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnShortMatrix(final short[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Int16}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Int16}([");
            for (int row = 0; row < rows; row++) {
                final short[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnInteger(final int value) {
        returnExpression("Int32(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnIntegerVector(final int[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Int32}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnIntegerMatrix(final int[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Int32}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Int32}([");
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnLong(final long value) {
        returnExpression("Int64(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnLongVector(final long[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Int64}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnLongMatrix(final long[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Int64}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Int64}([");
            for (int row = 0; row < rows; row++) {
                final long[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnFloat(final float value) {
        returnExpression("Float32(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnFloatVector(final float[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Float32}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnFloatMatrix(final float[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Float32}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Float32}([");
            for (int row = 0; row < rows; row++) {
                final float[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnDouble(final double value) {
        returnExpression("Float64(" + String.valueOf(value) + ")");
    }

    @Override
    public void returnDoubleVector(final double[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("Array{Float64}([");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(value[i]);
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnDoubleMatrix(final double[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("Array{Float64}(undef, " + value.length + ", 0)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("Array{Float64}([");
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append(";");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(" ");
                    }
                    sb.append(valueRow[col]);
                }
            }
            sb.append("])");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnNull() {
        returnExpression("()");
    }

}
