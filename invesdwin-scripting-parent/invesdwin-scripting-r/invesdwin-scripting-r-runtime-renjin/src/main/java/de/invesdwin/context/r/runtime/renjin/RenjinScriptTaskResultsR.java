package de.invesdwin.context.r.runtime.renjin;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.ScriptException;

import org.renjin.primitives.matrix.Matrix;
import org.renjin.sexp.Logical;
import org.renjin.sexp.SEXP;
import org.renjin.sexp.Vector;

import de.invesdwin.context.r.runtime.contract.IScriptTaskResultsR;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class RenjinScriptTaskResultsR implements IScriptTaskResultsR {

    private final RenjinScriptTaskEngineR engine;

    public RenjinScriptTaskResultsR(final RenjinScriptTaskEngineR engine) {
        this.engine = engine;
    }

    @Override
    public RenjinScriptTaskEngineR getEngine() {
        return engine;
    }

    @Override
    public String getString(final String variable) {
        try {
            final Vector sexp = (Vector) engine.unwrap().eval(variable);
            if (sexp.length() == 0 || sexp.isElementNA(0)) {
                return null;
            } else {
                return sexp.asString();
            }
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String[] getStringVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Vector sexp = (Vector) engine.unwrap().eval(variable);
                final String[] array = new String[sexp.length()];
                for (int i = 0; i < array.length; i++) {
                    array[i] = sexp.getElementAsString(i);
                }
                return array;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Matrix sexp = new Matrix((Vector) engine.unwrap().eval(variable));
                final String[][] matrix = new String[sexp.getNumRows()][];
                for (int row = 0; row < matrix.length; row++) {
                    final String[] vector = new String[sexp.getNumCols()];
                    for (int col = 0; col < vector.length; col++) {
                        vector[col] = sexp.getVector().getElementAsString(org.renjin.primitives.Indexes
                                .matrixIndexToVectorIndex(row, col, sexp.getNumRows(), sexp.getNumCols()));
                    }
                    matrix[row] = vector;
                }
                return matrix;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public double getDouble(final String variable) {
        try {
            final SEXP sexp = (SEXP) engine.unwrap().eval(variable);
            return sexp.asReal();
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Vector sexp = (Vector) engine.unwrap().eval(variable);
                final double[] array = new double[sexp.length()];
                for (int i = 0; i < array.length; i++) {
                    array[i] = sexp.getElementAsDouble(i);
                }
                return array;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Matrix sexp = new Matrix((Vector) engine.unwrap().eval(variable));
                final double[][] matrix = new double[sexp.getNumRows()][];
                for (int row = 0; row < matrix.length; row++) {
                    final double[] vector = new double[sexp.getNumCols()];
                    for (int col = 0; col < vector.length; col++) {
                        vector[col] = sexp.getElementAsDouble(row, col);
                    }
                    matrix[row] = vector;
                }
                return matrix;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public int getInteger(final String variable) {
        try {
            final Vector sexp = (Vector) engine.unwrap().eval(variable);
            Assertions.checkEquals(sexp.length(), 1);
            return sexp.getElementAsInt(0);
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Vector sexp = (Vector) engine.unwrap().eval(variable);
                final int[] array = new int[sexp.length()];
                for (int i = 0; i < array.length; i++) {
                    array[i] = sexp.getElementAsInt(i);
                }
                return array;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Matrix sexp = new Matrix((Vector) engine.unwrap().eval(variable));
                final int[][] matrix = new int[sexp.getNumRows()][];
                for (int row = 0; row < matrix.length; row++) {
                    final int[] vector = new int[sexp.getNumCols()];
                    for (int col = 0; col < vector.length; col++) {
                        vector[col] = sexp.getElementAsInt(row, col);
                    }
                    matrix[row] = vector;
                }
                return matrix;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public boolean getBoolean(final String variable) {
        try {
            final SEXP sexp = (SEXP) engine.unwrap().eval(variable);
            return sexp.asLogical() == Logical.TRUE;
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Vector sexp = (Vector) engine.unwrap().eval(variable);
                final boolean[] array = new boolean[sexp.length()];
                for (int i = 0; i < array.length; i++) {
                    array[i] = sexp.getElementAsLogical(i) == Logical.TRUE;
                }
                return array;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            try {
                final Matrix sexp = new Matrix((Vector) engine.unwrap().eval(variable));
                final boolean[][] matrix = new boolean[sexp.getNumRows()][];
                for (int row = 0; row < matrix.length; row++) {
                    final boolean[] vector = new boolean[sexp.getNumCols()];
                    for (int col = 0; col < vector.length; col++) {
                        vector[col] = sexp.getElementAsInt(row, col) > 0;
                    }
                    matrix[row] = vector;
                }
                return matrix;
            } catch (final ScriptException e) {
                throw new RuntimeException(e);
            }
        }
    }

}