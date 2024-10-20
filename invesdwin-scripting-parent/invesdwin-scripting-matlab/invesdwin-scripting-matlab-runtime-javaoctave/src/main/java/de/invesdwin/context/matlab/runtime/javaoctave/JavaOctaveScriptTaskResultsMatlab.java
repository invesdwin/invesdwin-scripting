package de.invesdwin.context.matlab.runtime.javaoctave;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskResultsMatlab;
import de.invesdwin.util.assertions.Assertions;
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
import dk.ange.octave.type.OctaveBoolean;
import dk.ange.octave.type.OctaveCell;
import dk.ange.octave.type.OctaveDouble;
import dk.ange.octave.type.OctaveInt;
import dk.ange.octave.type.OctaveObject;
import dk.ange.octave.type.OctaveString;

@NotThreadSafe
public class JavaOctaveScriptTaskResultsMatlab implements IScriptTaskResultsMatlab {

    private static final String INTERNAL_RESULT_VARIABLE_EXPRESSION = JavaOctaveScriptTaskRunnerMatlab.INTERNAL_RESULT_VARIABLE
            + "_expression";

    private final JavaOctaveScriptTaskEngineMatlab engine;

    public JavaOctaveScriptTaskResultsMatlab(final JavaOctaveScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JavaOctaveScriptTaskEngineMatlab getEngine() {
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
        final OctaveObject oo = engine.unwrap().get(requestVariable(variable));
        return unpack(oo);
    }

    private Object unpack(final OctaveObject obj) {
        if (obj instanceof OctaveDouble) {
            final OctaveDouble cObj = (OctaveDouble) obj;
            return unpackDouble(cObj);
        } else if (obj instanceof OctaveInt) {
            final OctaveInt cObj = (OctaveInt) obj;
            return unpackInt(cObj);
        } else if (obj instanceof OctaveBoolean) {
            final OctaveBoolean cObj = (OctaveBoolean) obj;
            return unpackBoolean(cObj);
        } else if (obj instanceof OctaveString) {
            final OctaveString cObj = (OctaveString) obj;
            String str = cObj.getString();
            if (Strings.isEmpty(str)) {
                str = null;
            }
            return str;
        } else if (obj instanceof OctaveCell) {
            final OctaveCell cObj = (OctaveCell) obj;
            return unpackCell(cObj);
        } else {
            throw UnknownArgumentException.newInstance(OctaveObject.class, obj);
        }
    }

    private Object unpackCell(final OctaveCell cObj) {
        final int[] size = cObj.getSize();
        Assertions.checkEquals(2, size.length, "Scalar, Vector or Matrix expected: %s", size);
        final int rows = size[0];
        final int cols = size[1];
        if (rows == 1 && cols == 1) {
            //scalar
            final OctaveObject scalar = cObj.get(1, 1);
            return unpack(scalar);
        } else if (rows == 1 || cols == 1) {
            //vector
            final String[] vector = new String[rows];
            for (int i = 0; i < vector.length; i++) {
                final OctaveString os = (OctaveString) cObj.get(i + 1);
                String str = os.getString();
                if (Strings.isEmpty(str)) {
                    str = null;
                }
                vector[i] = str;
            }
            return vector;
        } else {
            //matrix
            final String[][] matrix = new String[rows][];
            for (int row = 0; row < rows; row++) {
                final String[] vector = new String[cols];
                for (int col = 0; col < cols; col++) {
                    final OctaveString os = (OctaveString) cObj.get(row + 1, col + 1);
                    String str = os.getString();
                    if (Strings.isEmpty(str)) {
                        str = null;
                    }
                    vector[col] = str;
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    private Object unpackBoolean(final OctaveBoolean cObj) {
        final int[] size = cObj.getSize();
        Assertions.checkEquals(2, size.length, "Scalar, Vector or Matrix expected: %s", size);
        final int rows = size[0];
        final int cols = size[1];
        if (rows == 1 && cols == 1) {
            //scalar
            return cObj.getData()[0];
        } else if (rows == 1 || cols == 1) {
            //vector
            return cObj.getData();
        } else {
            //matrix
            final boolean[][] matrix = new boolean[rows][];
            for (int row = 0; row < rows; row++) {
                final boolean[] vector = new boolean[cols];
                for (int col = 0; col < cols; col++) {
                    vector[col] = cObj.get(row + 1, col + 1);
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    private Object unpackInt(final OctaveInt cObj) {
        final int[] size = cObj.getSize();
        Assertions.checkEquals(2, size.length, "Scalar, Vector or Matrix expected: %s", size);
        final int rows = size[0];
        final int cols = size[1];
        if (rows == 1 && cols == 1) {
            //scalar
            return cObj.getData()[0];
        } else if (rows == 1 || cols == 1) {
            //vector
            return cObj.getData();
        } else {
            //matrix
            final int[][] matrix = new int[rows][];
            for (int row = 0; row < rows; row++) {
                final int[] vector = new int[cols];
                for (int col = 0; col < cols; col++) {
                    vector[col] = cObj.get(row + 1, col + 1);
                }
                matrix[row] = vector;
            }
            return matrix;
        }
    }

    private Object unpackDouble(final OctaveDouble cObj) {
        final int[] size = cObj.getSize();
        Assertions.checkEquals(2, size.length, "Scalar, Vector or Matrix expected: %s", size);
        final int rows = size[0];
        final int cols = size[1];
        if (rows == 1 && cols == 1) {
            //scalar
            return cObj.getData()[0];
        } else if (rows == 1 || cols == 1) {
            //vector
            return cObj.getData();
        } else {
            //matrix
            final double[][] matrix = new double[rows][];
            for (int row = 0; row < rows; row++) {
                final double[] vector = new double[cols];
                for (int col = 0; col < cols; col++) {
                    vector[col] = cObj.get(row + 1, col + 1);
                }
                matrix[row] = vector;
            }
            return matrix;
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
        final Object obj = get("double(" + variable + ")");
        return Doubles.checkedCast(obj);
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        if (isNull(variable)) {
            return null;
        } else if (isEmpty(variable)) {
            return new double[0];
        } else {
            final Object obj = get("double(" + variable + ")");
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
            final Object obj = get("double(" + variable + ")");
            return Doubles.checkedCastMatrix(obj);
        }
    }

    @Override
    public int getInteger(final String variable) {
        return Integers.checkedCast(getDouble(variable));
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        return Integers.checkedCastVector(getDoubleVector(variable));
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        return Integers.checkedCastMatrix(getDoubleMatrix(variable));
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
        final int integerValue = getInteger(variable);
        return Bytes.checkedCast(integerValue);
    }

    @Override
    public byte[] getByteVector(final String variable) {
        final int[] integerValue = getIntegerVector(variable);
        return Bytes.checkedCastVector(integerValue);
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        final int[][] integerValue = getIntegerMatrix(variable);
        return Bytes.checkedCastMatrix(integerValue);
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
        final int integerValue = getInteger(variable);
        return Shorts.checkedCast(integerValue);
    }

    @Override
    public short[] getShortVector(final String variable) {
        final int[] integerValue = getIntegerVector(variable);
        return Shorts.checkedCastVector(integerValue);
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        final int[][] integerValue = getIntegerMatrix(variable);
        return Shorts.checkedCastMatrix(integerValue);
    }

    @Override
    public long getLong(final String variable) {
        final double doubleValue = getDouble(variable);
        return Longs.checkedCast(doubleValue);
    }

    @Override
    public long[] getLongVector(final String variable) {
        final double[] doubleValue = getDoubleVector(variable);
        return Longs.checkedCastVector(doubleValue);
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        final double[][] doubleValue = getDoubleMatrix(variable);
        return Longs.checkedCastMatrix(doubleValue);
    }

    @Override
    public boolean isNull(final String variable) {
        return getBoolean("isnumeric(" + variable + ") && isnan(" + variable + ")");
    }

    public boolean isEmpty(final String variable) {
        return getBoolean("isempty(" + variable + ")");
    }

}