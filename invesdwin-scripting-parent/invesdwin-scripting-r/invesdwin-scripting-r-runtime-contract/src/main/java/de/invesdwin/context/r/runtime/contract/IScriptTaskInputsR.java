package de.invesdwin.context.r.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskInputs;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Doubles;
import de.invesdwin.util.math.Integers;

public interface IScriptTaskInputsR extends IScriptTaskInputs {

    @Override
    default void putByte(final String variable, final byte value) {
        putInteger(variable, value);
    }

    @Override
    default void putByteVector(final String variable, final byte[] value) {
        final int[] integerValue = Integers.checkedCastVector(value);
        putIntegerVector(variable, integerValue);
    }

    @Override
    default void putByteMatrix(final String variable, final byte[][] value) {
        final int[][] integerValue = Integers.checkedCastMatrix(value);
        putIntegerMatrix(variable, integerValue);
    }

    @Override
    default void putCharacter(final String variable, final char value) {
        final String stringValue = Strings.checkedCast(value);
        putString(variable, stringValue);
    }

    @Override
    default void putCharacterVector(final String variable, final char[] value) {
        final String[] stringValue = Strings.checkedCastVector(value);
        putStringVector(variable, stringValue);
    }

    @Override
    default void putCharacterMatrix(final String variable, final char[][] value) {
        final String[][] stringValue = Strings.checkedCastMatrix(value);
        putStringMatrix(variable, stringValue);
    }

    @Override
    default void putFloat(final String variable, final float value) {
        putDouble(variable, value);
    }

    @Override
    default void putFloatVector(final String variable, final float[] value) {
        final double[] doubleValue = Doubles.checkedCastVector(value);
        putDoubleVector(variable, doubleValue);
    }

    @Override
    default void putFloatMatrix(final String variable, final float[][] value) {
        final double[][] doubleValue = Doubles.checkedCastMatrix(value);
        putDoubleMatrix(variable, doubleValue);
    }

    @Override
    default void putShort(final String variable, final short value) {
        putInteger(variable, value);
    }

    @Override
    default void putShortVector(final String variable, final short[] value) {
        final int[] integerValue = Integers.checkedCastVector(value);
        putIntegerVector(variable, integerValue);
    }

    @Override
    default void putShortMatrix(final String variable, final short[][] value) {
        final int[][] integerValue = Integers.checkedCastMatrix(value);
        putIntegerMatrix(variable, integerValue);
    }

    @Override
    default void putLong(final String variable, final long value) {
        putDouble(variable, value);
    }

    @Override
    default void putLongVector(final String variable, final long[] value) {
        final double[] doubleValue = Doubles.checkedCastVector(value);
        putDoubleVector(variable, doubleValue);
    }

    @Override
    default void putLongMatrix(final String variable, final long[][] value) {
        final double[][] doubleValue = Doubles.checkedCastMatrix(value);
        putDoubleMatrix(variable, doubleValue);
    }

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval(variable + " <- " + expression);
    }

    @Override
    default void putNull(final String variable) {
        putExpression(variable, "NA");
    }

    @Override
    default void remove(final String variable) {
        getEngine().eval("rm(list=c(\"" + variable + "\"))");
    }

}
