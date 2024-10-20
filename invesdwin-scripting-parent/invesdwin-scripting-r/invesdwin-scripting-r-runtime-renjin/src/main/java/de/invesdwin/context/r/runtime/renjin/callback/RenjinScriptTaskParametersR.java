package de.invesdwin.context.r.runtime.renjin.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

import org.renjin.primitives.matrix.Matrix;
import org.renjin.sexp.Logical;
import org.renjin.sexp.LogicalVector;
import org.renjin.sexp.SEXP;
import org.renjin.sexp.Vector;

import de.invesdwin.context.r.runtime.contract.callback.IScriptTaskParametersR;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class RenjinScriptTaskParametersR implements IScriptTaskParametersR, Closeable {

    private SEXP parameters;

    public void setParameters(final SEXP parameters) {
        this.parameters = parameters;
    }

    protected SEXP getParameter(final int index) {
        return parameters.getElementAsSEXP(index);
    }

    @Override
    public int size() {
        return parameters.length();
    }

    @Override
    public boolean isEmpty(final int index) {
        return getParameter(index).length() == 0;
    }

    @Override
    public boolean isNull(final int index) {
        final SEXP parameter = getParameter(index);
        if (parameter instanceof LogicalVector) {
            final LogicalVector cParameter = (LogicalVector) parameter;
            if (cParameter.length() == 1 && cParameter.getElementAsRawLogical(0) == LogicalVector.NA) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String getString(final int index) {
        final Vector sexp = (Vector) getParameter(index);
        if (sexp.length() == 0 || sexp.isElementNA(0)) {
            return null;
        } else {
            return sexp.asString();
        }
    }

    @Override
    public String[] getStringVector(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Vector sexp = (Vector) getParameter(index);
            final String[] array = new String[sexp.length()];
            for (int i = 0; i < array.length; i++) {
                array[i] = sexp.getElementAsString(i);
            }
            return array;
        }
    }

    @Override
    public String[][] getStringMatrix(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Matrix sexp = new Matrix((Vector) getParameter(index));
            final String[][] matrix = new String[sexp.getNumRows()][];
            for (int row = 0; row < matrix.length; row++) {
                final String[] vector = new String[sexp.getNumCols()];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = sexp.getVector()
                            .getElementAsString(org.renjin.primitives.Indexes.matrixIndexToVectorIndex(row, col,
                                    sexp.getNumRows(), sexp.getNumCols()));
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    @Override
    public double getDouble(final int index) {
        final SEXP sexp = getParameter(index);
        return sexp.asReal();
    }

    @Override
    public double[] getDoubleVector(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Vector sexp = (Vector) getParameter(index);
            final double[] array = new double[sexp.length()];
            for (int i = 0; i < array.length; i++) {
                array[i] = sexp.getElementAsDouble(i);
            }
            return array;
        }
    }

    @Override
    public double[][] getDoubleMatrix(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Matrix sexp = new Matrix((Vector) getParameter(index));
            final double[][] matrix = new double[sexp.getNumRows()][];
            for (int row = 0; row < matrix.length; row++) {
                final double[] vector = new double[sexp.getNumCols()];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = sexp.getElementAsDouble(row, col);
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    @Override
    public int getInteger(final int index) {
        final Vector sexp = (Vector) getParameter(index);
        Assertions.checkEquals(sexp.length(), 1);
        return sexp.getElementAsInt(0);
    }

    @Override
    public int[] getIntegerVector(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Vector sexp = (Vector) getParameter(index);
            final int[] array = new int[sexp.length()];
            for (int i = 0; i < array.length; i++) {
                array[i] = sexp.getElementAsInt(i);
            }
            return array;
        }
    }

    @Override
    public int[][] getIntegerMatrix(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Matrix sexp = new Matrix((Vector) getParameter(index));
            final int[][] matrix = new int[sexp.getNumRows()][];
            for (int row = 0; row < matrix.length; row++) {
                final int[] vector = new int[sexp.getNumCols()];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = sexp.getElementAsInt(row, col);
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    @Override
    public boolean getBoolean(final int index) {
        final SEXP sexp = getParameter(index);
        return sexp.asLogical() == Logical.TRUE;
    }

    @Override
    public boolean[] getBooleanVector(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Vector sexp = (Vector) getParameter(index);
            final boolean[] array = new boolean[sexp.length()];
            for (int i = 0; i < array.length; i++) {
                array[i] = sexp.getElementAsLogical(i) == Logical.TRUE;
            }
            return array;
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final int index) {
        if (isNull(index)) {
            return null;
        } else {
            final Matrix sexp = new Matrix((Vector) getParameter(index));
            final boolean[][] matrix = new boolean[sexp.getNumRows()][];
            for (int row = 0; row < matrix.length; row++) {
                final boolean[] vector = new boolean[sexp.getNumCols()];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = sexp.getElementAsInt(row, col) > 0;
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    @Override
    public void close() {
        parameters = null;
    }

    @Override
    public String toString() {
        return Strings.asString(parameters);
    }

}