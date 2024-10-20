package de.invesdwin.context.matlab.runtime.javasci.callback.file;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.matlab.runtime.contract.callback.ScriptTaskReturnsMatlabToExpression;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.math.Doubles;

@NotThreadSafe
public class JavasciScriptTaskReturnsMatlabToExpression extends ScriptTaskReturnsMatlabToExpression {

    @Override
    public void returnNull() {
        returnExpression("%nan");
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
                    sb.append("''");
                } else {
                    sb.append("'");
                    sb.append(v);
                    sb.append("'");
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
                        sb.append("''");
                    } else {
                        sb.append("'");
                        sb.append(v);
                        sb.append("'");
                    }
                }
            }
            sb.append("]");
            returnExpression(sb.toString());
        }
    }

    @Override
    public void returnFloat(final float value) {
        returnDouble(value);
    }

    @Override
    public void returnFloatVector(final float[] value) {
        returnDoubleVector(Doubles.checkedCastVector(value));
    }

    @Override
    public void returnFloatMatrix(final float[][] value) {
        returnDoubleMatrix(Doubles.checkedCastMatrix(value));
    }

    @Override
    public void returnBoolean(final boolean value) {
        if (value) {
            returnExpression("%T");
        } else {
            returnExpression("%F");
        }
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
                if (v) {
                    sb.append("%T");
                } else {
                    sb.append("%F");
                }
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
                    if (v) {
                        sb.append("%T");
                    } else {
                        sb.append("%F");
                    }
                }
            }
            sb.append("]");
            returnExpression(sb.toString());
        }
    }

}
