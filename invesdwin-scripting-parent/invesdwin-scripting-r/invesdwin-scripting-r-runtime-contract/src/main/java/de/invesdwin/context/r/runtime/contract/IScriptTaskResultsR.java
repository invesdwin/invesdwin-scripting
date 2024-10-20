package de.invesdwin.context.r.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskResults;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Floats;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;

public interface IScriptTaskResultsR extends IScriptTaskResults {

    @Override
    default byte getByte(final String variable) {
        final int integerValue = getInteger(variable);
        return Bytes.checkedCast(integerValue);
    }

    @Override
    default byte[] getByteVector(final String variable) {
        final int[] integerValue = getIntegerVector(variable);
        return Bytes.checkedCastVector(integerValue);
    }

    @Override
    default byte[][] getByteMatrix(final String variable) {
        final int[][] integerValue = getIntegerMatrix(variable);
        return Bytes.checkedCastMatrix(integerValue);
    }

    @Override
    default char getCharacter(final String variable) {
        final String doubleValue = getString(variable);
        return Characters.checkedCast(doubleValue);
    }

    @Override
    default char[] getCharacterVector(final String variable) {
        final String[] doubleValue = getStringVector(variable);
        return Characters.checkedCastVector(doubleValue);
    }

    @Override
    default char[][] getCharacterMatrix(final String variable) {
        final String[][] doubleValue = getStringMatrix(variable);
        return Characters.checkedCastMatrix(doubleValue);
    }

    @Override
    default float getFloat(final String variable) {
        final double doubleValue = getDouble(variable);
        return Floats.checkedCast(doubleValue);
    }

    @Override
    default float[] getFloatVector(final String variable) {
        final double[] doubleValue = getDoubleVector(variable);
        return Floats.checkedCastVector(doubleValue);
    }

    @Override
    default float[][] getFloatMatrix(final String variable) {
        final double[][] doubleValue = getDoubleMatrix(variable);
        return Floats.checkedCastMatrix(doubleValue);
    }

    @Override
    default short getShort(final String variable) {
        final int integerValue = getInteger(variable);
        return Shorts.checkedCast(integerValue);
    }

    @Override
    default short[] getShortVector(final String variable) {
        final int[] integerValue = getIntegerVector(variable);
        return Shorts.checkedCastVector(integerValue);
    }

    @Override
    default short[][] getShortMatrix(final String variable) {
        final int[][] integerValue = getIntegerMatrix(variable);
        return Shorts.checkedCastMatrix(integerValue);
    }

    @Override
    default long getLong(final String variable) {
        final double doubleValue = getDouble(variable);
        return Longs.checkedCast(doubleValue);
    }

    @Override
    default long[] getLongVector(final String variable) {
        final double[] doubleValue = getDoubleVector(variable);
        return Longs.checkedCastVector(doubleValue);
    }

    @Override
    default long[][] getLongMatrix(final String variable) {
        final double[][] doubleValue = getDoubleMatrix(variable);
        return Longs.checkedCastMatrix(doubleValue);
    }

    @Override
    default boolean isDefined(final String variable) {
        return getBoolean("exists(\"" + variable + "\")");
    }

    @Override
    default boolean isNull(final String variable) {
        return getBoolean("length(" + variable + ") != 0 && all(is.na(" + variable + "))");
    }

    default boolean isEmpty(final String variable) {
        return getBoolean("length(" + variable + ") == 0");
    }

}
