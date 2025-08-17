package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.matlab.runtime.contract.AScriptTaskInputsMatlabToExpression;
import de.invesdwin.scripting.matlab.runtime.jascib.callback.file.JascibScriptTaskReturnsMatlabToExpression;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.math.Doubles;

@NotThreadSafe
public class JascibScriptTaskInputsMatlab extends AScriptTaskInputsMatlabToExpression {

    private final JascibScriptTaskEngineMatlab engine;

    public JascibScriptTaskInputsMatlab(final JascibScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JascibScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    @Override
    public void putNull(final String variable) {
        putExpression(variable, "%nan");
    }

    @Override
    protected String getFloatType() {
        return "double";
    }

    @Override
    protected String booleanToString(final boolean value) {
        return JascibScriptTaskReturnsMatlabToExpression.booleanToString(value);
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
                    sb.append("''");
                } else {
                    sb.append("'");
                    sb.append(v);
                    sb.append("'");
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
                        sb.append("''");
                    } else {
                        sb.append("'");
                        sb.append(v);
                        sb.append("'");
                    }
                }
            }
            sb.append("]");
            putExpression(variable, sb.toString());
        }
    }

    @Override
    protected void putEmptyRows(final String variable, final int rows) {
        final StringBuilder sb = new StringBuilder("list(");
        for (int i = 0; i < rows; i++) {
            if (i > 0) {
                sb.append(",");
            }
            sb.append("[]");
        }
        sb.append(")");
        putExpression(variable, sb.toString());
    }

    @Override
    protected String doubleToString(final double value) {
        if (Doubles.isNaN(value)) {
            return "%nan";
        } else {
            return super.doubleToString(value);
        }
    }

}
