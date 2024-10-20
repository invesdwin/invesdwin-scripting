package de.invesdwin.context.julia.runtime.libjuliaclj.internal;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.concurrent.future.Futures;
import de.invesdwin.util.concurrent.lock.IReentrantLock;

/**
 * Always acquire the lock first before accessing the julia engine instance. Also make sure commands are only executed
 * from inside the EXECUTOR thread. Otherwise julia will throw errors due to being thread bound.
 * 
 * https://cnuernber.github.io/libjulia-clj/signals.html
 */
@NotThreadSafe
public final class InitializingJuliaEngineWrapper implements IJuliaEngineWrapper {

    private static final InitializingJuliaEngineWrapper INSTANCE = new InitializingJuliaEngineWrapper();
    private boolean initialized = false;

    private InitializingJuliaEngineWrapper() {
    }

    public static boolean isInitialized() {
        return INSTANCE.initialized;
    }

    private void maybeInit() {
        if (initialized) {
            return;
        }
        synchronized (this) {
            if (initialized) {
                return;
            }
            if (UncheckedJuliaEngineWrapper.EXECUTOR.isExecutorThread()) {
                UncheckedJuliaEngineWrapper.INSTANCE.init();
            } else {
                if (UncheckedJuliaEngineWrapper.EXECUTOR.getPendingCount() > 0) {
                    throw new IllegalStateException(
                            "Initialization should already be taking place, would deadlock if another thread tried it!");
                }
                Futures.waitNoInterrupt(
                        UncheckedJuliaEngineWrapper.EXECUTOR.submit(UncheckedJuliaEngineWrapper.INSTANCE::init));
            }
            initialized = true;
        }

    }

    @Override
    public void eval(final String command) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.eval(command);
    }

    @Override
    public JsonNode getAsJsonNode(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getAsJsonNode(variable);
    }

    @Override
    public void putByteVector(final String variable, final byte[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putByteVector(variable, vector);
    }

    @Override
    public byte[] getByteVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getByteVector(variable);
    }

    @Override
    public void putShortVector(final String variable, final short[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putShortVector(variable, vector);
    }

    @Override
    public short[] getShortVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getShortVector(variable);
    }

    @Override
    public void putIntegerVector(final String variable, final int[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putIntegerVector(variable, vector);
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getIntegerVector(variable);
    }

    @Override
    public void putLongVector(final String variable, final long[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putLongVector(variable, vector);
    }

    @Override
    public long[] getLongVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getLongVector(variable);
    }

    @Override
    public void putFloatVector(final String variable, final float[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putFloatVector(variable, vector);
    }

    @Override
    public float[] getFloatVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getFloatVector(variable);
    }

    @Override
    public void putDoubleVector(final String variable, final double[] vector) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putDoubleVector(variable, vector);
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getDoubleVector(variable);
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putByteMatrix(variable, matrix);
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getByteMatrix(variable);
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putShortMatrix(variable, matrix);
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getShortMatrix(variable);
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putIntegerMatrix(variable, matrix);
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getIntegerMatrix(variable);
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putLongMatrix(variable, matrix);
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getLongMatrix(variable);
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putFloatMatrix(variable, matrix);
    }

    @Override
    public float[][] getFloatMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getFloatMatrix(variable);
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] matrix) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putDoubleMatrix(variable, matrix);
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getDoubleMatrix(variable);
    }

    @Override
    public String[] getStringVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getStringVectorAsJson(variable);
    }

    @Override
    public String[][] getStringMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getStringMatrixAsJson(variable);
    }

    @Override
    public char[] getCharacterVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getCharacterVectorAsJson(variable);
    }

    @Override
    public char[][] getCharacterMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getCharacterMatrixAsJson(variable);
    }

    @Override
    public boolean[] getBooleanVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getBooleanVectorAsJson(variable);
    }

    @Override
    public boolean[][] getBooleanMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getBooleanMatrixAsJson(variable);
    }

    @Override
    public byte[] getByteVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getByteVectorAsJson(variable);
    }

    @Override
    public byte[][] getByteMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getByteMatrixAsJson(variable);
    }

    @Override
    public short[] getShortVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getShortVectorAsJson(variable);
    }

    @Override
    public short[][] getShortMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getShortMatrixAsJson(variable);
    }

    @Override
    public int[] getIntegerVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getIntegerVectorAsJson(variable);
    }

    @Override
    public int[][] getIntegerMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getIntegerMatrixAsJson(variable);
    }

    @Override
    public long[] getLongVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getLongVectorAsJson(variable);
    }

    @Override
    public long[][] getLongMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getLongMatrixAsJson(variable);
    }

    @Override
    public float[] getFloatVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getFloatVectorAsJson(variable);
    }

    @Override
    public float[][] getFloatMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getFloatMatrixAsJson(variable);
    }

    @Override
    public double[] getDoubleVectorAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getDoubleVectorAsJson(variable);
    }

    @Override
    public double[][] getDoubleMatrixAsJson(final String variable) {
        maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE.getDoubleMatrixAsJson(variable);
    }

    @Override
    public void putCharacterVectorAsString(final String variable, final char[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putCharacterVectorAsString(variable, value);
    }

    @Override
    public void putCharacterMatrixAsString(final String variable, final char[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putCharacterMatrixAsString(variable, value);
    }

    @Override
    public void putStringVectorAsString(final String variable, final String[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putStringVectorAsString(variable, value);
    }

    @Override
    public void putStringMatrixAsString(final String variable, final String[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putStringMatrixAsString(variable, value);
    }

    @Override
    public void putBooleanVectorAsString(final String variable, final boolean[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putBooleanVectorAsString(variable, value);
    }

    @Override
    public void putBooleanMatrixAsString(final String variable, final boolean[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putBooleanMatrixAsString(variable, value);
    }

    @Override
    public void putByteVectorAsString(final String variable, final byte[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putByteVectorAsString(variable, value);
    }

    @Override
    public void putByteMatrixAsString(final String variable, final byte[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putByteMatrixAsString(variable, value);
    }

    @Override
    public void putShortVectorAsString(final String variable, final short[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putShortVectorAsString(variable, value);
    }

    @Override
    public void putShortMatrixAsString(final String variable, final short[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putShortMatrixAsString(variable, value);
    }

    @Override
    public void putIntegerVectorAsString(final String variable, final int[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putIntegerVectorAsString(variable, value);
    }

    @Override
    public void putIntegerMatrixAsString(final String variable, final int[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putIntegerMatrixAsString(variable, value);
    }

    @Override
    public void putLongVectorAsString(final String variable, final long[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putLongVectorAsString(variable, value);
    }

    @Override
    public void putLongMatrixAsString(final String variable, final long[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putLongMatrixAsString(variable, value);
    }

    @Override
    public void putFloatVectorAsString(final String variable, final float[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putFloatVectorAsString(variable, value);
    }

    @Override
    public void putFloatMatrixAsString(final String variable, final float[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putFloatMatrixAsString(variable, value);
    }

    @Override
    public void putDoubleVectorAsString(final String variable, final double[] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putDoubleVectorAsString(variable, value);
    }

    @Override
    public void putDoubleMatrixAsString(final String variable, final double[][] value) {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.putDoubleMatrixAsString(variable, value);
    }

    @Override
    public void reset() {
        maybeInit();
        UncheckedJuliaEngineWrapper.INSTANCE.reset();
    }

    @Override
    public IReentrantLock getLock() {
        return UncheckedJuliaEngineWrapper.INSTANCE.getLock();
    }

    public static IJuliaEngineWrapper getInstance() {
        INSTANCE.maybeInit();
        return UncheckedJuliaEngineWrapper.INSTANCE;
    }

}
