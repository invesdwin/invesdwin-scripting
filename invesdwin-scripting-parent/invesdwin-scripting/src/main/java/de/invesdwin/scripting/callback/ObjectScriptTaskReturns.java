package de.invesdwin.scripting.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class ObjectScriptTaskReturns implements IScriptTaskReturns, Closeable {

    private Object returnValue;
    private boolean returnExpression;

    public Object getReturnValue() {
        return returnValue;
    }

    public boolean isReturnExpression() {
        return returnExpression;
    }

    public ObjectScriptTaskReturnValue newReturn() {
        return new ObjectScriptTaskReturnValue(returnExpression, returnValue);
    }

    protected void returnValue(final Object value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnByte(final byte value) {
        returnValue(value);
    }

    @Override
    public void returnByteVector(final byte[] value) {
        returnValue(value);
    }

    @Override
    public void returnByteMatrix(final byte[][] value) {
        returnValue(value);
    }

    @Override
    public void returnCharacter(final char value) {
        returnValue(value);
    }

    @Override
    public void returnCharacterVector(final char[] value) {
        returnValue(value);
    }

    @Override
    public void returnCharacterMatrix(final char[][] value) {
        returnValue(value);
    }

    @Override
    public void returnString(final String value) {
        returnValue(value);
    }

    @Override
    public void returnStringVector(final String[] value) {
        returnValue(value);
    }

    @Override
    public void returnStringMatrix(final String[][] value) {
        returnValue(value);
    }

    @Override
    public void returnFloat(final float value) {
        returnValue(value);
    }

    @Override
    public void returnFloatVector(final float[] value) {
        returnValue(value);
    }

    @Override
    public void returnFloatMatrix(final float[][] value) {
        returnValue(value);
    }

    @Override
    public void returnDouble(final double value) {
        returnValue(value);
    }

    @Override
    public void returnDoubleVector(final double[] value) {
        returnValue(value);
    }

    @Override
    public void returnDoubleMatrix(final double[][] value) {
        returnValue(value);
    }

    @Override
    public void returnShort(final short value) {
        returnValue(value);
    }

    @Override
    public void returnShortVector(final short[] value) {
        returnValue(value);
    }

    @Override
    public void returnShortMatrix(final short[][] value) {
        returnValue(value);
    }

    @Override
    public void returnInteger(final int value) {
        returnValue(value);
    }

    @Override
    public void returnIntegerVector(final int[] value) {
        returnValue(value);
    }

    @Override
    public void returnIntegerMatrix(final int[][] value) {
        returnValue(value);
    }

    @Override
    public void returnLong(final long value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnLongVector(final long[] value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnLongMatrix(final long[][] value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnBoolean(final boolean value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnBooleanVector(final boolean[] value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnBooleanMatrix(final boolean[][] value) {
        assert returnValue == null;
        this.returnValue = value;
    }

    @Override
    public void returnExpression(final String expression) {
        assert returnValue == null;
        this.returnValue = expression;
        this.returnExpression = true;
    }

    @Override
    public void returnNull() {
        assert returnValue == null;
        returnValue = null;
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
