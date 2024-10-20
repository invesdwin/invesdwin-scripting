package de.invesdwin.context.matlab.runtime.javasci;

import javax.annotation.concurrent.NotThreadSafe;

import org.scilab.modules.javasci.JavasciException;
import org.scilab.modules.types.ScilabBoolean;
import org.scilab.modules.types.ScilabDouble;
import org.scilab.modules.types.ScilabInteger;
import org.scilab.modules.types.ScilabString;
import org.scilab.modules.types.ScilabType;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskResultsMatlab;
import de.invesdwin.util.error.UnknownArgumentException;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Doubles;
import de.invesdwin.util.math.Floats;
import de.invesdwin.util.math.Integers;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;

@NotThreadSafe
public class JavasciScriptTaskResultsMatlab implements IScriptTaskResultsMatlab {

    private static final String INTERNAL_RESULT_VARIABLE_EXPRESSION = JavasciScriptTaskRunnerMatlab.INTERNAL_RESULT_VARIABLE
            + "_expression";

    private final JavasciScriptTaskEngineMatlab engine;

    public JavasciScriptTaskResultsMatlab(final JavasciScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JavasciScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    private String requestVariable(final String variable) {
        if (Strings.containsAny(variable, '[', '.', '(', '{')) {
            //we have to support expressions here
            engine.eval(INTERNAL_RESULT_VARIABLE_EXPRESSION + " = " + variable);
            return INTERNAL_RESULT_VARIABLE_EXPRESSION;
        } else {
            return variable;
        }
    }

    private Object get(final String variable) {
        try {
            final ScilabType st = engine.unwrap().getScilab().get(requestVariable(variable));
            return unpack(st);
        } catch (final JavasciException e) {
            throw new RuntimeException(e);
        }
    }

    private Object unpack(final ScilabType obj) {
        if (obj instanceof ScilabDouble) {
            final ScilabDouble cObj = (ScilabDouble) obj;
            return cObj.getRawRealPart();
        } else if (obj instanceof ScilabInteger) {
            final ScilabInteger cObj = (ScilabInteger) obj;
            return cObj.getRawData();
        } else if (obj instanceof ScilabBoolean) {
            final ScilabBoolean cObj = (ScilabBoolean) obj;
            return cObj.getRawData();
        } else if (obj instanceof ScilabString) {
            final ScilabString cObj = (ScilabString) obj;
            replaceEmptyWithNull(cObj);
            return cObj.getData();
        } else {
            throw UnknownArgumentException.newInstance(ScilabType.class, obj);
        }
    }

    private void replaceEmptyWithNull(final ScilabString str) {
        for (final String[] v : str.getData()) {
            for (int i = 0; i < v.length; i++) {
                if (Strings.isEmpty(v[i])) {
                    v[i] = null;
                }
            }
        }
    }

    @Override
    public String getString(final String variable) {
        if (isNull(variable)) {
            return null;
        } else {
            final Object obj = get(variable);
            return Strings.checkedCast(obj);
        }
    }

    @Override
    public String[] getStringVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new String[0];
        } else {
            final Object obj = get(variable);
            return Strings.checkedCastVector(obj);
        }
    }

    @Override
    public String[][] getStringMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final String[][] matrix = new String[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new String[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Strings.checkedCastMatrix(obj);
        }
    }

    @Override
    public double getDouble(final String variable) {
        final Object obj = get(variable);
        return Doubles.checkedCast(obj);
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new double[0];
        } else {
            final Object obj = get(variable);
            return Doubles.checkedCastVector(obj);
        }
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final double[][] matrix = new double[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new double[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Doubles.checkedCastMatrix(obj);
        }
    }

    @Override
    public int getInteger(final String variable) {
        final Object obj = get(variable);
        return Integers.checkedCast(obj);
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new int[0];
        } else {
            final Object obj = get(variable);
            return Integers.checkedCastVector(obj);
        }
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final int[][] matrix = new int[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new int[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Integers.checkedCastMatrix(obj);
        }
    }

    @Override
    public boolean getBoolean(final String variable) {
        final Object obj = get(variable);
        return Booleans.checkedCast(obj);
    }

    @Override
    public boolean[] getBooleanVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new boolean[0];
        } else {
            final Object obj = get(variable);
            return Booleans.checkedCastVector(obj);
        }
    }

    @Override
    public boolean[][] getBooleanMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final boolean[][] matrix = new boolean[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new boolean[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Booleans.checkedCastMatrix(obj);
        }
    }

    @Override
    public byte getByte(final String variable) {
        final Object obj = get(variable);
        return Bytes.checkedCast(obj);
    }

    @Override
    public byte[] getByteVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new byte[0];
        } else {
            final Object obj = get(variable);
            return Bytes.checkedCastVector(obj);
        }
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final byte[][] matrix = new byte[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new byte[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Bytes.checkedCastMatrix(obj);
        }
    }

    @Override
    public char getCharacter(final String variable) {
        final String doubleValue = getString(variable);
        return Characters.checkedCast(doubleValue);
    }

    @Override
    public char[] getCharacterVector(final String variable) {
        final String[] doubleValue = getStringVector(variable);
        return Characters.checkedCastVector(doubleValue);
    }

    @Override
    public char[][] getCharacterMatrix(final String variable) {
        final String[][] doubleValue = getStringMatrix(variable);
        return Characters.checkedCastMatrix(doubleValue);
    }

    @Override
    public float getFloat(final String variable) {
        final double doubleValue = getDouble(variable);
        return Floats.checkedCast(doubleValue);
    }

    @Override
    public float[] getFloatVector(final String variable) {
        final double[] doubleValue = getDoubleVector(variable);
        return Floats.checkedCastVector(doubleValue);
    }

    @Override
    public float[][] getFloatMatrix(final String variable) {
        final double[][] doubleValue = getDoubleMatrix(variable);
        return Floats.checkedCastMatrix(doubleValue);
    }

    @Override
    public short getShort(final String variable) {
        final Object obj = get(variable);
        return Shorts.checkedCast(obj);
    }

    @Override
    public short[] getShortVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new short[0];
        } else {
            final Object obj = get(variable);
            return Shorts.checkedCastVector(obj);
        }
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final short[][] matrix = new short[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new short[0];
            }
            return matrix;
        } else {
            final Object obj = get(variable);
            return Shorts.checkedCastMatrix(obj);
        }
    }

    @Override
    public long getLong(final String variable) {
        //UnsupportedTypeException: 64 bit (signed and unsigned) integer types not managed in Scilab 5.X
        final Object obj = get("double(" + variable + ")");
        return Longs.checkedCast(obj);
    }

    @Override
    public long[] getLongVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new long[0];
        } else {
            final Object obj = get("double(" + variable + ")");
            return Longs.checkedCastVector(obj);
        }
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            final int rows = getInteger("size(" + variable + ",1)");
            final long[][] matrix = new long[rows][];
            for (int i = 0; i < rows; i++) {
                matrix[i] = new long[0];
            }
            return matrix;
        } else {
            final Object obj = get("double(" + variable + ")");
            return Longs.checkedCastMatrix(obj);
        }
    }

    @Override
    public boolean isDefined(final String variable) {
        return getBoolean("exists('" + variable + "')");
    }

    @Override
    public boolean isNull(final String variable) {
        return getBoolean("~isempty(" + variable + ") & and(isnan(" + variable + "))");
    }

    public boolean isEmpty(final String variable) {
        return getBoolean("isempty(" + variable + ")");
    }

}