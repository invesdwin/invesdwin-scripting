package de.invesdwin.context.r.runtime.contract.callback;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public abstract class AScriptTaskReturnsRToExpression implements IScriptTaskReturnsR {

    @Override
    public void returnString(final String value) {
        if (value == null) {
            returnExpression("NA_character_");
        } else {
            returnExpression("'" + value + "'");
        }
    }

    @Override
    public void returnStringVector(final String[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("c(");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final String v = value[i];
                if (v == null) {
                    sb.append("NA_character_");
                } else {
                    sb.append("'");
                    sb.append(v);
                    sb.append("'");
                }
            }
            sb.append(")");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnStringMatrix(final String[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("matrix(character(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("c(c(");
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("),c(");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final String v = valueRow[col];
                    if (v == null) {
                        sb.append("NA_character_");
                    } else {
                        sb.append("\"");
                        sb.append(v);
                        sb.append("\"");
                    }
                }
            }
            sb.append("))");
            returnExpression("matrix(" + sb.toString() + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

    @Override
    public void returnBoolean(final boolean value) {
        if (value) {
            returnExpression("TRUE");
        } else {
            returnExpression("FALSE");
        }
    }

    @Override
    public void returnBooleanVector(final boolean[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("c(");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final boolean v = value[i];
                if (v) {
                    sb.append("TRUE");
                } else {
                    sb.append("FALSE");
                }
            }
            sb.append(")");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnBooleanMatrix(final boolean[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("matrix(logical(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("c(c(");
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("),c(");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final boolean v = valueRow[col];
                    if (v) {
                        sb.append("TRUE");
                    } else {
                        sb.append("FALSE");
                    }
                }
            }
            sb.append("))");
            returnExpression("matrix(" + sb.toString() + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

    @Override
    public void returnInteger(final int value) {
        returnExpression(String.valueOf(value) + "L");
    }

    @Override
    public void returnIntegerVector(final int[] value) {
        if (value == null) {
            returnNull();
        } else {
            final StringBuilder sb = new StringBuilder("c(");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final int v = value[i];
                sb.append(v);
                sb.append("L");
            }
            sb.append(")");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnIntegerMatrix(final int[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("matrix(integer(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("c(c(");
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("),c(");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final int v = valueRow[col];
                    sb.append(v);
                    sb.append("L");
                }
            }
            sb.append("))");
            returnExpression("matrix(" + sb.toString() + ", " + rows + ", " + cols + ", TRUE)");
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
        } else {
            final StringBuilder sb = new StringBuilder("c(");
            for (int i = 0; i < value.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final double v = value[i];
                sb.append(v);
            }
            sb.append(")");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnDoubleMatrix(final double[][] value) {
        if (value == null) {
            returnNull();
        } else if (value.length == 0 || value[0].length == 0) {
            returnExpression("matrix(double(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final StringBuilder sb = new StringBuilder("c(c(");
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                if (row > 0) {
                    sb.append("),c(");
                }
                for (int col = 0; col < cols; col++) {
                    if (col > 0) {
                        sb.append(",");
                    }
                    final double v = valueRow[col];
                    sb.append(v);
                }
            }
            sb.append("))");
            returnExpression("matrix(" + sb.toString() + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

}
