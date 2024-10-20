package de.invesdwin.context.r.runtime.contract.callback;

import de.invesdwin.context.integration.script.callback.IScriptTaskReturns;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Doubles;
import de.invesdwin.util.math.Integers;

public interface IScriptTaskReturnsR extends IScriptTaskReturns {

    @Override
    default void returnByte(final byte value) {
        returnInteger(value);
    }

    @Override
    default void returnByteVector(final byte[] value) {
        final int[] integerValue = Integers.checkedCastVector(value);
        returnIntegerVector(integerValue);
    }

    @Override
    default void returnByteMatrix(final byte[][] value) {
        final int[][] integerValue = Integers.checkedCastMatrix(value);
        returnIntegerMatrix(integerValue);
    }

    @Override
    default void returnCharacter(final char value) {
        final String stringValue = Strings.checkedCast(value);
        returnString(stringValue);
    }

    @Override
    default void returnCharacterVector(final char[] value) {
        final String[] stringValue = Strings.checkedCastVector(value);
        returnStringVector(stringValue);
    }

    @Override
    default void returnCharacterMatrix(final char[][] value) {
        final String[][] stringValue = Strings.checkedCastMatrix(value);
        returnStringMatrix(stringValue);
    }

    @Override
    default void returnFloat(final float value) {
        returnDouble(value);
    }

    @Override
    default void returnFloatVector(final float[] value) {
        final double[] doubleValue = Doubles.checkedCastVector(value);
        returnDoubleVector(doubleValue);
    }

    @Override
    default void returnFloatMatrix(final float[][] value) {
        final double[][] doubleValue = Doubles.checkedCastMatrix(value);
        returnDoubleMatrix(doubleValue);
    }

    @Override
    default void returnShort(final short value) {
        returnInteger(value);
    }

    @Override
    default void returnShortVector(final short[] value) {
        final int[] integerValue = Integers.checkedCastVector(value);
        returnIntegerVector(integerValue);
    }

    @Override
    default void returnShortMatrix(final short[][] value) {
        final int[][] integerValue = Integers.checkedCastMatrix(value);
        returnIntegerMatrix(integerValue);
    }

    @Override
    default void returnLong(final long value) {
        returnDouble(value);
    }

    @Override
    default void returnLongVector(final long[] value) {
        final double[] doubleValue = Doubles.checkedCastVector(value);
        returnDoubleVector(doubleValue);
    }

    @Override
    default void returnLongMatrix(final long[][] value) {
        final double[][] doubleValue = Doubles.checkedCastMatrix(value);
        returnDoubleMatrix(doubleValue);
    }

    @Override
    default void returnNull() {
        returnExpression("NA");
    }

}
