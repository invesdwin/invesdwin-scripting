package de.invesdwin.context.r.runtime.contract.callback;

import de.invesdwin.context.integration.script.callback.IScriptTaskParameters;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Floats;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;

public interface IScriptTaskParametersR extends IScriptTaskParameters {

    @Override
    default byte getByte(final int index) {
        final int integerValue = getInteger(index);
        return Bytes.checkedCast(integerValue);
    }

    @Override
    default byte[] getByteVector(final int index) {
        final int[] integerValue = getIntegerVector(index);
        return Bytes.checkedCastVector(integerValue);
    }

    @Override
    default byte[][] getByteMatrix(final int index) {
        final int[][] integerValue = getIntegerMatrix(index);
        return Bytes.checkedCastMatrix(integerValue);
    }

    @Override
    default char getCharacter(final int index) {
        final String doubleValue = getString(index);
        return Characters.checkedCast(doubleValue);
    }

    @Override
    default char[] getCharacterVector(final int index) {
        final String[] doubleValue = getStringVector(index);
        return Characters.checkedCastVector(doubleValue);
    }

    @Override
    default char[][] getCharacterMatrix(final int index) {
        final String[][] doubleValue = getStringMatrix(index);
        return Characters.checkedCastMatrix(doubleValue);
    }

    @Override
    default float getFloat(final int index) {
        final double doubleValue = getDouble(index);
        return Floats.checkedCast(doubleValue);
    }

    @Override
    default float[] getFloatVector(final int index) {
        final double[] doubleValue = getDoubleVector(index);
        return Floats.checkedCastVector(doubleValue);
    }

    @Override
    default float[][] getFloatMatrix(final int index) {
        final double[][] doubleValue = getDoubleMatrix(index);
        return Floats.checkedCastMatrix(doubleValue);
    }

    @Override
    default short getShort(final int index) {
        final int integerValue = getInteger(index);
        return Shorts.checkedCast(integerValue);
    }

    @Override
    default short[] getShortVector(final int index) {
        final int[] integerValue = getIntegerVector(index);
        return Shorts.checkedCastVector(integerValue);
    }

    @Override
    default short[][] getShortMatrix(final int index) {
        final int[][] integerValue = getIntegerMatrix(index);
        return Shorts.checkedCastMatrix(integerValue);
    }

    @Override
    default long getLong(final int index) {
        final double doubleValue = getDouble(index);
        return Longs.checkedCast(doubleValue);
    }

    @Override
    default long[] getLongVector(final int index) {
        final double[] doubleValue = getDoubleVector(index);
        return Longs.checkedCastVector(doubleValue);
    }

    @Override
    default long[][] getLongMatrix(final int index) {
        final double[][] doubleValue = getDoubleMatrix(index);
        return Longs.checkedCastMatrix(doubleValue);
    }

    boolean isEmpty(int index);

}
