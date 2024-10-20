package de.invesdwin.context.r.runtime.rcaller;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.r.runtime.contract.IScriptTaskResultsR;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class RCallerScriptTaskResultsR implements IScriptTaskResultsR {

    private static final String INTERNAL_RESULT_VARIABLE_EXPRESSION = RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE
            + "_expression";

    private final RCallerScriptTaskEngineR engine;

    public RCallerScriptTaskResultsR(final RCallerScriptTaskEngineR engine) {
        this.engine = engine;
    }

    @Override
    public RCallerScriptTaskEngineR getEngine() {
        return engine;
    }

    private String requestVariable(final String variable) {
        if (Strings.containsAny(variable, '[', '.', '(', '{')) {
            //we have to support expressions here
            engine.unwrap().getRCode().addRCode(INTERNAL_RESULT_VARIABLE_EXPRESSION + " <- " + variable);
            engine.unwrap().runAndReturnResultOnline(INTERNAL_RESULT_VARIABLE_EXPRESSION);
            return INTERNAL_RESULT_VARIABLE_EXPRESSION;
        } else {
            engine.unwrap().runAndReturnResultOnline(variable);
            return variable;
        }
    }

    @Override
    public String getString(final String variable) {
        final String[] array = replaceNaWithNull(
                engine.unwrap().getParser().getAsStringArray(requestVariable(variable)));
        Assertions.checkEquals(array.length, 1);
        return array[0];
    }

    private String[] replaceNaWithNull(final String[] array) {
        for (int i = 0; i < array.length; i++) {
            if ("NA".equals(array[i])) {
                array[i] = null;
            }
        }
        return array;
    }

    @Override
    public String[] getStringVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new String[0];
        } else {
            return replaceNaWithNull(engine.unwrap().getParser().getAsStringArray(requestVariable(variable)));
        }
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        if (isEmpty(variable)) {
            final int rows = getInteger("nrow(" + variable + ")");
            final String[][] matrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new String[0];
            }
            return matrix;
        } else {
            final String[] ct = getStringVector(variable);
            if (ct == null) {
                return null;
            }
            engine.unwrap().getRCode().addRCode(
                    RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE + " <- dim(" + variable + ")");
            final int[] ds = getIntegerVector(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE);
            if ((ds == null) || (ds.length != 2)) {
                return null;
            }
            final int m = ds[0];
            final int n = ds[1];
            final String[][] r = new String[m][n];

            int i = 0;
            int k = 0;
            while (i < n) {
                int j = 0;
                while (j < m) {
                    r[(j++)][i] = ct[(k++)];
                }
                i++;
            }
            return r;
        }
    }

    @Override
    public double getDouble(final String variable) {
        final double[] array = engine.unwrap().getParser().getAsDoubleArray(requestVariable(variable));
        Assertions.checkEquals(array.length, 1);
        return array[0];
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new double[0];
        } else {
            return engine.unwrap().getParser().getAsDoubleArray(requestVariable(variable));
        }
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        if (isEmpty(variable)) {
            final int rows = getInteger("nrow(" + variable + ")");
            final double[][] matrix = new double[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new double[0];
            }
            return matrix;
        } else {
            //not using engine getDoubleMatrix since it transposes the matrix as a side effect...
            final double[] ct = getDoubleVector(variable);
            if (ct == null) {
                return null;
            }
            final int[] ds = engine.unwrap().getParser().getDimensions(variable);
            if ((ds == null) || (ds.length != 2)) {
                return null;
            }
            final int m = ds[0];
            final int n = ds[1];
            final double[][] r = new double[m][n];

            int i = 0;
            int k = 0;
            while (i < n) {
                int j = 0;
                while (j < m) {
                    r[(j++)][i] = ct[(k++)];
                }
                i++;
            }
            return r;
        }
    }

    @Override
    public int getInteger(final String variable) {
        final int[] array = engine.unwrap().getParser().getAsIntArray(requestVariable(variable));
        Assertions.checkEquals(array.length, 1);
        return array[0];
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new int[0];
        } else {
            return engine.unwrap().getParser().getAsIntArray(requestVariable(variable));
        }
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        if (isEmpty(variable)) {
            final int rows = getInteger("nrow(" + variable + ")");
            final int[][] matrix = new int[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new int[0];
            }
            return matrix;
        } else {
            final int[] ct = getIntegerVector(variable);
            if (ct == null) {
                return null;
            }
            final int[] ds = engine.unwrap().getParser().getDimensions(variable);
            if ((ds == null) || (ds.length != 2)) {
                return null;
            }
            final int m = ds[0];
            final int n = ds[1];
            final int[][] r = new int[m][n];

            int i = 0;
            int k = 0;
            while (i < n) {
                int j = 0;
                while (j < m) {
                    r[(j++)][i] = ct[(k++)];
                }
                i++;
            }
            return r;
        }
    }

    @Override
    public boolean getBoolean(final String variable) {
        final boolean[] array = engine.unwrap().getParser().getAsLogicalArray(requestVariable(variable));
        Assertions.checkEquals(array.length, 1);
        return array[0];
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new boolean[0];
        } else {
            return engine.unwrap().getParser().getAsLogicalArray(requestVariable(variable));
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            engine.unwrap().getRCode().addRCode(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE
                    + " <- array(as.numeric(" + variable + "), dim(" + variable + "))");
            final double[][] matrix = getDoubleMatrix(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE);
            final boolean[][] booleanMatrix = new boolean[matrix.length][];
            for (int i = 0; i < matrix.length; i++) {
                final double[] vector = matrix[i];
                final boolean[] booleanVector = new boolean[vector.length];
                for (int j = 0; j < vector.length; j++) {
                    booleanVector[j] = vector[j] > 0;
                }
                booleanMatrix[i] = booleanVector;
            }
            return booleanMatrix;
        }
    }

}