package de.invesdwin.context.r.runtime.rserve;

import javax.annotation.concurrent.NotThreadSafe;

import org.math.R.Rsession.RException;

import de.invesdwin.context.r.runtime.contract.IScriptTaskInputsR;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class RserveScriptTaskInputsR implements IScriptTaskInputsR {

    private final RserveScriptTaskEngineR engine;

    public RserveScriptTaskInputsR(final RserveScriptTaskEngineR engine) {
        this.engine = engine;
    }

    @Override
    public RserveScriptTaskEngineR getEngine() {
        return engine;
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putExpression(variable, "NA_character_");
        } else {
            try {
                engine.unwrap().set(variable, value);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            try {
                engine.unwrap().set(variable, value);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
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
        try {
            engine.unwrap().set(variable, value);
        } catch (final RException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            try {
                engine.unwrap().set(variable, value);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(double(), " + value.length + ", 0, TRUE)");
        } else {
            try {
                engine.unwrap().set(variable, value);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final RException e) {
            throw new RuntimeException(e);
        }
        putExpression(variable, "as.integer(" + variable + ")");
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final double[] doubleValue = new double[value.length];
            for (int i = 0; i < doubleValue.length; i++) {
                doubleValue[i] = value[i];
            }
            try {
                engine.unwrap().set(variable, doubleValue);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
            putExpression(variable, "as.integer(" + variable + ")");
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(integer(), " + value.length + ", 0, TRUE)");
        } else {
            final double[][] doubleValue = new double[value.length][];
            for (int i = 0; i < value.length; i++) {
                final int[] vector = value[i];
                final double[] doubleVector = new double[vector.length];
                for (int j = 0; j < vector.length; j++) {
                    doubleVector[j] = vector[j];
                }
                doubleValue[i] = doubleVector;
            }
            try {
                engine.unwrap().set(variable, doubleValue);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
            putExpression(variable, "array(as.integer(" + variable + "), dim(" + variable + "))");
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        final double doubleValue;
        if (value) {
            doubleValue = 1D;
        } else {
            doubleValue = 0D;
        }
        try {
            engine.unwrap().set(variable, doubleValue);
        } catch (final RException e) {
            throw new RuntimeException(e);
        }
        putExpression(variable, "as.logical(" + variable + ")");
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            final double[] doubleValue = new double[value.length];
            for (int i = 0; i < value.length; i++) {
                if (value[i]) {
                    doubleValue[i] = 1D;
                } else {
                    doubleValue[i] = 0D;
                }
            }
            try {
                engine.unwrap().set(variable, doubleValue);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
            putExpression(variable, "as.logical(" + variable + ")");
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "matrix(logical(), " + value.length + ", 0, TRUE)");
        } else {
            final double[][] doubleValue = new double[value.length][];
            for (int i = 0; i < value.length; i++) {
                final boolean[] vector = value[i];
                final double[] doubleVector = new double[vector.length];
                for (int j = 0; j < vector.length; j++) {
                    if (vector[j]) {
                        doubleVector[j] = 1D;
                    } else {
                        doubleVector[j] = 0D;
                    }
                }
                doubleValue[i] = doubleVector;
            }
            try {
                engine.unwrap().set(variable, doubleValue);
            } catch (final RException e) {
                throw new RuntimeException(e);
            }
            putExpression(variable, "array(as.logical(" + variable + "), dim(" + variable + "))");
        }
    }

}
