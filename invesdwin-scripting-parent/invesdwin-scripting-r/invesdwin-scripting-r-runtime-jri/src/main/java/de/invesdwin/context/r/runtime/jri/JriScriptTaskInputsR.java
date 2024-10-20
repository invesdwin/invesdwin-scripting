package de.invesdwin.context.r.runtime.jri;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.r.runtime.contract.IScriptTaskInputsR;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class JriScriptTaskInputsR implements IScriptTaskInputsR {

    private final JriScriptTaskEngineR engine;

    public JriScriptTaskInputsR(final JriScriptTaskEngineR engine) {
        this.engine = engine;
    }

    @Override
    public JriScriptTaskEngineR getEngine() {
        return engine;
    }

    @Override
    public void putString(final String variable, final String value) {
        Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, value));
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, value));
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(character(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final String[] flatMatrix = new String[rows * cols];
            int i = 0;
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                for (int col = 0; col < cols; col++) {
                    flatMatrix[i] = valueRow[col];
                    i++;
                }
            }
            putStringVector(variable, flatMatrix);
            putExpression(variable, "matrix(" + variable + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, new double[] { value }));
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, value));
        }
    }

    /**
     * http://permalink.gmane.org/gmane.comp.lang.r.rosuda.devel/87
     */
    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(double(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final double[] flatMatrix = new double[rows * cols];
            int i = 0;
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                for (int col = 0; col < cols; col++) {
                    flatMatrix[i] = valueRow[col];
                    i++;
                }
            }
            engine.unwrap().getRengine().assign(variable, flatMatrix);
            putExpression(variable, "matrix(" + variable + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, new int[] { value }));
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, value));
        }
    }

    /**
     * http://permalink.gmane.org/gmane.comp.lang.r.rosuda.devel/87
     */
    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(integer(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final int[] flatMatrix = new int[rows * cols];
            int i = 0;
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                for (int col = 0; col < cols; col++) {
                    flatMatrix[i] = valueRow[col];
                    i++;
                }
            }
            engine.unwrap().getRengine().assign(variable, flatMatrix);
            putExpression(variable, "matrix(" + variable + ", " + rows + ", " + cols + ", TRUE)");
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, new boolean[] { value }));
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            Assertions.checkTrue(engine.unwrap().getRengine().assign(variable, value));
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(logical(), " + value.length + ", 0, TRUE)");
        } else {
            final int rows = value.length;
            final int cols = value[0].length;
            final int[] flatMatrix = new int[rows * cols];
            int i = 0;
            for (int row = 0; row < rows; row++) {
                final boolean[] valueRow = value[row];
                Assertions.checkEquals(valueRow.length, cols);
                for (int col = 0; col < cols; col++) {
                    if (valueRow[col]) {
                        flatMatrix[i] = 1;
                    } else {
                        flatMatrix[i] = 0;
                    }
                    i++;
                }
            }
            engine.unwrap().getRengine().assign(variable, flatMatrix);
            putExpression(variable, "matrix(" + variable + ", " + rows + ", " + cols + ", TRUE)");
            putExpression(variable, "array(as.logical(" + variable + "), dim(" + variable + "))");
        }
    }

}
