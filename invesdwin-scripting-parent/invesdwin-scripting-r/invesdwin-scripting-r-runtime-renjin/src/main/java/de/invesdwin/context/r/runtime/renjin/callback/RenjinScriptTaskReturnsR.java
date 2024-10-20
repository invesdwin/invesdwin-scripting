package de.invesdwin.context.r.runtime.renjin.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

import org.renjin.invoke.reflection.converters.Converter;
import org.renjin.invoke.reflection.converters.StringConverter;
import org.renjin.primitives.matrix.DoubleMatrixBuilder;
import org.renjin.primitives.matrix.IntMatrixBuilder;
import org.renjin.primitives.matrix.StringMatrixBuilder;
import org.renjin.sexp.LogicalArrayVector;
import org.renjin.sexp.SEXP;

import de.invesdwin.context.r.runtime.contract.callback.IScriptTaskReturnsR;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.loadingcache.ALoadingCache;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class RenjinScriptTaskReturnsR implements IScriptTaskReturnsR, Closeable {

    @SuppressWarnings("rawtypes")
    private static final ALoadingCache<Class<?>, Converter> CONVERTERS = new ALoadingCache<Class<?>, Converter>() {
        @Override
        protected Converter loadValue(final Class<?> key) {
            return org.renjin.invoke.reflection.converters.Converters.get(key);
        }
    };

    private SEXP returnValue;
    private boolean returnExpression;

    @SuppressWarnings({ "rawtypes", "unchecked" })
    protected void returnValue(final Object value) {
        assert returnValue == null;
        final Converter converter = CONVERTERS.get(value.getClass());
        final SEXP converted = converter.convertToR(value);
        this.returnValue = converted;
    }

    protected void returnValue(final SEXP value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnString(final String value) {
        if (value == null) {
            returnExpression("NA_character_");
        } else {
            returnValue(value);
        }
    }

    @Override
    public void returnStringVector(final String[] value) {
        if (value == null) {
            returnNull();
        } else {
            returnValue(value);
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
            final StringMatrixBuilder matrix = new StringMatrixBuilder(rows, cols);
            for (int row = 0; row < rows; row++) {
                final String[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    matrix.setValue(row, col, valueRow[col]);
                }
            }
            returnValue(matrix.build());
        }
    }

    @Override
    public void returnDouble(final double value) {
        returnValue(value);
    }

    @Override
    public void returnDoubleVector(final double[] value) {
        if (value == null) {
            returnNull();
        } else {
            returnValue(value);
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
            final DoubleMatrixBuilder matrix = new DoubleMatrixBuilder(rows, cols);
            for (int row = 0; row < rows; row++) {
                final double[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    matrix.setValue(row, col, valueRow[col]);
                }
            }
            returnValue(matrix.build());
        }
    }

    @Override
    public void returnInteger(final int value) {
        returnValue(value);
    }

    @Override
    public void returnIntegerVector(final int[] value) {
        if (value == null) {
            returnNull();
        } else {
            returnValue(value);
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
            final IntMatrixBuilder matrix = new IntMatrixBuilder(rows, cols);
            for (int row = 0; row < rows; row++) {
                final int[] valueRow = value[row];
                for (int col = 0; col < cols; col++) {
                    matrix.setValue(row, col, valueRow[col]);
                }
            }
            returnValue(matrix.build());
        }
    }

    @Override
    public void returnBoolean(final boolean value) {
        returnValue(value);
    }

    @Override
    public void returnBooleanVector(final boolean[] value) {
        if (value == null) {
            returnNull();
        } else {
            returnValue(new LogicalArrayVector(value));
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
    public void returnByte(final byte value) {
        IScriptTaskReturnsR.super.returnByte(value);
    }

    @Override
    public void returnByteVector(final byte[] value) {
        IScriptTaskReturnsR.super.returnByteVector(value);
    }

    @Override
    public void returnByteMatrix(final byte[][] value) {
        IScriptTaskReturnsR.super.returnByteMatrix(value);
    }

    @Override
    public void returnCharacter(final char value) {
        IScriptTaskReturnsR.super.returnCharacter(value);
    }

    @Override
    public void returnCharacterVector(final char[] value) {
        IScriptTaskReturnsR.super.returnCharacterVector(value);
    }

    @Override
    public void returnCharacterMatrix(final char[][] value) {
        IScriptTaskReturnsR.super.returnCharacterMatrix(value);
    }

    @Override
    public void returnFloat(final float value) {
        IScriptTaskReturnsR.super.returnFloat(value);
    }

    @Override
    public void returnFloatVector(final float[] value) {
        IScriptTaskReturnsR.super.returnFloatVector(value);
    }

    @Override
    public void returnFloatMatrix(final float[][] value) {
        IScriptTaskReturnsR.super.returnFloatMatrix(value);
    }

    @Override
    public void returnShort(final short value) {
        IScriptTaskReturnsR.super.returnShort(value);
    }

    @Override
    public void returnShortVector(final short[] value) {
        IScriptTaskReturnsR.super.returnShortVector(value);
    }

    @Override
    public void returnShortMatrix(final short[][] value) {
        IScriptTaskReturnsR.super.returnShortMatrix(value);
    }

    @Override
    public void returnLong(final long value) {
        IScriptTaskReturnsR.super.returnLong(value);
    }

    @Override
    public void returnLongVector(final long[] value) {
        IScriptTaskReturnsR.super.returnLongVector(value);
    }

    @Override
    public void returnLongMatrix(final long[][] value) {
        IScriptTaskReturnsR.super.returnLongMatrix(value);
    }

    @Override
    public void returnNull() {
        IScriptTaskReturnsR.super.returnNull();
    }

    public SEXP getReturnValue() {
        return returnValue;
    }

    public boolean isReturnExpression() {
        return returnExpression;
    }

    public SexpScriptTaskReturnValue newReturn() {
        return new SexpScriptTaskReturnValue(returnExpression, returnValue);
    }

    @Override
    public void returnExpression(final String expression) {
        assert returnValue == null;
        this.returnValue = StringConverter.INSTANCE.convertToR(expression);
        this.returnExpression = true;
    }

    @Override
    public void close() {
        returnValue = null;
        returnExpression = false;
    }

    @Override
    public String toString() {
        return Strings.asString(returnValue);
    }

}
