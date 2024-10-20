package de.invesdwin.context.r.runtime.jri;

import javax.annotation.concurrent.NotThreadSafe;

import org.rosuda.JRI.RBool;
import org.rosuda.JRI.REXP;

import de.invesdwin.context.r.runtime.contract.IScriptTaskResultsR;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class JriScriptTaskResultsR implements IScriptTaskResultsR {

    private final JriScriptTaskEngineR engine;

    public JriScriptTaskResultsR(final JriScriptTaskEngineR engine) {
        this.engine = engine;
    }

    @Override
    public JriScriptTaskEngineR getEngine() {
        return engine;
    }

    @Override
    public String getString(final String variable) {
        final REXP rexp = engine.unwrap().getRengine().eval(variable);
        return rexp.asString();
    }

    @Override
    public String[] getStringVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new String[0];
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return rexp.asStringArray();
        }
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return asStringMatrix(rexp);
        }
    }

    private String[][] asStringMatrix(final REXP rexp) {
        final String[] ct = rexp.asStringArray();
        if (ct == null) {
            return null;
        }
        final REXP dim = rexp.getAttribute("dim");
        if (dim == null) {
            return null;
        }
        final int[] ds = dim.asIntArray();
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

    @Override
    public double getDouble(final String variable) {
        final REXP rexp = engine.unwrap().getRengine().eval(variable);
        return rexp.asDouble();
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new double[0];
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return rexp.asDoubleArray();
        }
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return rexp.asDoubleMatrix();
        }
    }

    @Override
    public int getInteger(final String variable) {
        final REXP rexp = engine.unwrap().getRengine().eval(variable);
        final int[] array = rexp.asIntArray();
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
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return rexp.asIntArray();
        }
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            return asIntMatrix(rexp);
        }
    }

    private int[][] asIntMatrix(final REXP rexp) {
        final int[] ct = rexp.asIntArray();
        if (ct == null) {
            return null;
        }
        final REXP dim = rexp.getAttribute("dim");
        if (dim == null) {
            return null;
        }
        final int[] ds = dim.asIntArray();
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

    @Override
    public boolean getBoolean(final String variable) {
        final REXP rexp = engine.unwrap().getRengine().eval(variable);
        final RBool bool = rexp.asBool();
        return bool.isTRUE();
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new boolean[0];
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            final int[] boolArray = rexp.asIntArray();
            final boolean[] booleanVector = new boolean[boolArray.length];
            for (int i = 0; i < boolArray.length; i++) {
                booleanVector[i] = boolArray[i] > 0;
            }
            return booleanVector;
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final REXP rexp = engine.unwrap().getRengine().eval(variable);
            final int[][] matrix = asIntMatrix(rexp);
            final boolean[][] booleanMatrix = new boolean[matrix.length][];
            for (int i = 0; i < matrix.length; i++) {
                final int[] vector = matrix[i];
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