package de.invesdwin.context.julia.runtime.libjuliaclj.internal;

import java.util.concurrent.Future;

import javax.annotation.concurrent.ThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.future.Futures;
import de.invesdwin.util.concurrent.lock.IReentrantLock;

@ThreadSafe
public final class ExecutorJuliaEngineWrapper implements IJuliaEngineWrapper {

    private static final ExecutorJuliaEngineWrapper UNCHECKED = new ExecutorJuliaEngineWrapper(
            UncheckedJuliaEngineWrapper.INSTANCE);

    private final IJuliaEngineWrapper delegate;
    private final WrappedExecutorService executor;

    private ExecutorJuliaEngineWrapper(final IJuliaEngineWrapper delegate) {
        this.delegate = InitializingJuliaEngineWrapper.getInstance();
        this.executor = UncheckedJuliaEngineWrapper.EXECUTOR;
    }

    public IJuliaEngineWrapper getDelegate() {
        return delegate;
    }

    public WrappedExecutorService getExecutor() {
        return executor;
    }

    @Override
    public void eval(final String command) {
        final Future<?> future = executor.submit(() -> delegate.eval(command));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public JsonNode getAsJsonNode(final String variable) {
        final Future<JsonNode> future = executor.submit(() -> delegate.getAsJsonNode(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putByteVector(final String variable, final byte[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putByteVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public byte[] getByteVector(final String variable) {
        final Future<byte[]> future = executor.submit(() -> delegate.getByteVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putShortVector(final String variable, final short[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putShortVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public short[] getShortVector(final String variable) {
        final Future<short[]> future = executor.submit(() -> delegate.getShortVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putIntegerVector(final String variable, final int[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putIntegerVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public int[] getIntegerVector(final String variable) {
        final Future<int[]> future = executor.submit(() -> delegate.getIntegerVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putLongVector(final String variable, final long[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putLongVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public long[] getLongVector(final String variable) {
        final Future<long[]> future = executor.submit(() -> delegate.getLongVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putFloatVector(final String variable, final float[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putFloatVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public float[] getFloatVector(final String variable) {
        final Future<float[]> future = executor.submit(() -> delegate.getFloatVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putDoubleVector(final String variable, final double[] vector) {
        final Future<?> future = executor.submit(() -> delegate.putDoubleVector(variable, vector));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public double[] getDoubleVector(final String variable) {
        final Future<double[]> future = executor.submit(() -> delegate.getDoubleVector(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putByteMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public byte[][] getByteMatrix(final String variable) {
        final Future<byte[][]> future = executor.submit(() -> delegate.getByteMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putShortMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public short[][] getShortMatrix(final String variable) {
        final Future<short[][]> future = executor.submit(() -> delegate.getShortMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putIntegerMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public int[][] getIntegerMatrix(final String variable) {
        final Future<int[][]> future = executor.submit(() -> delegate.getIntegerMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putLongMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public long[][] getLongMatrix(final String variable) {
        final Future<long[][]> future = executor.submit(() -> delegate.getLongMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putFloatMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public float[][] getFloatMatrix(final String variable) {
        final Future<float[][]> future = executor.submit(() -> delegate.getFloatMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] matrix) {
        final Future<?> future = executor.submit(() -> delegate.putDoubleMatrix(variable, matrix));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public double[][] getDoubleMatrix(final String variable) {
        final Future<double[][]> future = executor.submit(() -> delegate.getDoubleMatrix(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public String[] getStringVectorAsJson(final String variable) {
        final Future<String[]> future = executor.submit(() -> delegate.getStringVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public String[][] getStringMatrixAsJson(final String variable) {
        final Future<String[][]> future = executor.submit(() -> delegate.getStringMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public char[] getCharacterVectorAsJson(final String variable) {
        final Future<char[]> future = executor.submit(() -> delegate.getCharacterVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public char[][] getCharacterMatrixAsJson(final String variable) {
        final Future<char[][]> future = executor.submit(() -> delegate.getCharacterMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public boolean[] getBooleanVectorAsJson(final String variable) {
        final Future<boolean[]> future = executor.submit(() -> delegate.getBooleanVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public boolean[][] getBooleanMatrixAsJson(final String variable) {
        final Future<boolean[][]> future = executor.submit(() -> delegate.getBooleanMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public byte[] getByteVectorAsJson(final String variable) {
        final Future<byte[]> future = executor.submit(() -> delegate.getByteVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public byte[][] getByteMatrixAsJson(final String variable) {
        final Future<byte[][]> future = executor.submit(() -> delegate.getByteMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public short[] getShortVectorAsJson(final String variable) {
        final Future<short[]> future = executor.submit(() -> delegate.getShortVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public short[][] getShortMatrixAsJson(final String variable) {
        final Future<short[][]> future = executor.submit(() -> delegate.getShortMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public int[] getIntegerVectorAsJson(final String variable) {
        final Future<int[]> future = executor.submit(() -> delegate.getIntegerVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public int[][] getIntegerMatrixAsJson(final String variable) {
        final Future<int[][]> future = executor.submit(() -> delegate.getIntegerMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public long[] getLongVectorAsJson(final String variable) {
        final Future<long[]> future = executor.submit(() -> delegate.getLongVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public long[][] getLongMatrixAsJson(final String variable) {
        final Future<long[][]> future = executor.submit(() -> delegate.getLongMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public float[] getFloatVectorAsJson(final String variable) {
        final Future<float[]> future = executor.submit(() -> delegate.getFloatVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public float[][] getFloatMatrixAsJson(final String variable) {
        final Future<float[][]> future = executor.submit(() -> delegate.getFloatMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public double[] getDoubleVectorAsJson(final String variable) {
        final Future<double[]> future = executor.submit(() -> delegate.getDoubleVectorAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public double[][] getDoubleMatrixAsJson(final String variable) {
        final Future<double[][]> future = executor.submit(() -> delegate.getDoubleMatrixAsJson(variable));
        return Futures.getNoInterrupt(future);
    }

    @Override
    public void putCharacterVectorAsString(final String variable, final char[] value) {
        final Future<?> future = executor.submit(() -> delegate.putCharacterVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putCharacterMatrixAsString(final String variable, final char[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putCharacterMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putStringVectorAsString(final String variable, final String[] value) {
        final Future<?> future = executor.submit(() -> delegate.putStringVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putStringMatrixAsString(final String variable, final String[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putStringMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putBooleanVectorAsString(final String variable, final boolean[] value) {
        final Future<?> future = executor.submit(() -> delegate.putBooleanVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putBooleanMatrixAsString(final String variable, final boolean[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putBooleanMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putByteVectorAsString(final String variable, final byte[] value) {
        final Future<?> future = executor.submit(() -> delegate.putByteVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putByteMatrixAsString(final String variable, final byte[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putByteMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putShortVectorAsString(final String variable, final short[] value) {
        final Future<?> future = executor.submit(() -> delegate.putShortVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putShortMatrixAsString(final String variable, final short[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putShortMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putIntegerVectorAsString(final String variable, final int[] value) {
        final Future<?> future = executor.submit(() -> delegate.putIntegerVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putIntegerMatrixAsString(final String variable, final int[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putIntegerMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putLongVectorAsString(final String variable, final long[] value) {
        final Future<?> future = executor.submit(() -> delegate.putLongVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putLongMatrixAsString(final String variable, final long[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putLongMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putFloatVectorAsString(final String variable, final float[] value) {
        final Future<?> future = executor.submit(() -> delegate.putFloatVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putFloatMatrixAsString(final String variable, final float[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putFloatMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putDoubleVectorAsString(final String variable, final double[] value) {
        final Future<?> future = executor.submit(() -> delegate.putDoubleVectorAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void putDoubleMatrixAsString(final String variable, final double[][] value) {
        final Future<?> future = executor.submit(() -> delegate.putDoubleMatrixAsString(variable, value));
        Futures.waitNoInterrupt(future);
    }

    @Override
    public void reset() {
        final Future<?> future = executor.submit(() -> delegate.reset());
        Futures.waitNoInterrupt(future);
    }

    @Override
    public IReentrantLock getLock() {
        return delegate.getLock();
    }

    public static ExecutorJuliaEngineWrapper getInstance() {
        if (InitializingJuliaEngineWrapper.isInitialized()) {
            return UNCHECKED;
        } else {
            return new ExecutorJuliaEngineWrapper(InitializingJuliaEngineWrapper.getInstance());
        }
    }

}
