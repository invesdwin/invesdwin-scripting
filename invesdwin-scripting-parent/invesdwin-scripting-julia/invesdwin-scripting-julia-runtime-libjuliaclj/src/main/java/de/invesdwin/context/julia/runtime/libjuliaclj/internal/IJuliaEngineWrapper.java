package de.invesdwin.context.julia.runtime.libjuliaclj.internal;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.concurrent.lock.IReentrantLock;

public interface IJuliaEngineWrapper {

    void eval(String command);

    JsonNode getAsJsonNode(String variable);

    void reset();

    IReentrantLock getLock();

    void putByteVector(String variable, byte[] vector);

    byte[] getByteVector(String variable);

    void putShortVector(String variable, short[] vector);

    short[] getShortVector(String variable);

    void putIntegerVector(String variable, int[] vector);

    int[] getIntegerVector(String variable);

    void putLongVector(String variable, long[] vector);

    long[] getLongVector(String variable);

    void putFloatVector(String variable, float[] vector);

    float[] getFloatVector(String variable);

    void putDoubleVector(String variable, double[] vector);

    double[] getDoubleVector(String variable);

    void putByteMatrix(String variable, byte[][] matrix);

    byte[][] getByteMatrix(String variable);

    void putShortMatrix(String variable, short[][] matrix);

    short[][] getShortMatrix(String variable);

    void putIntegerMatrix(String variable, int[][] matrix);

    int[][] getIntegerMatrix(String variable);

    void putLongMatrix(String variable, long[][] matrix);

    long[][] getLongMatrix(String variable);

    void putFloatMatrix(String variable, float[][] matrix);

    float[][] getFloatMatrix(String variable);

    void putDoubleMatrix(String variable, double[][] matrix);

    double[][] getDoubleMatrix(String variable);

    String[] getStringVectorAsJson(String variable);

    String[][] getStringMatrixAsJson(String variable);

    char[] getCharacterVectorAsJson(String variable);

    char[][] getCharacterMatrixAsJson(String variable);

    boolean[] getBooleanVectorAsJson(String variable);

    boolean[][] getBooleanMatrixAsJson(String variable);

    byte[] getByteVectorAsJson(String variable);

    byte[][] getByteMatrixAsJson(String variable);

    short[] getShortVectorAsJson(String variable);

    short[][] getShortMatrixAsJson(String variable);

    int[] getIntegerVectorAsJson(String variable);

    int[][] getIntegerMatrixAsJson(String variable);

    long[] getLongVectorAsJson(String variable);

    long[][] getLongMatrixAsJson(String variable);

    float[] getFloatVectorAsJson(String variable);

    float[][] getFloatMatrixAsJson(String variable);

    double[] getDoubleVectorAsJson(String variable);

    double[][] getDoubleMatrixAsJson(String variable);

    void putCharacterVectorAsString(String variable, char[] value);

    void putCharacterMatrixAsString(String variable, char[][] value);

    void putStringVectorAsString(String variable, String[] value);

    void putStringMatrixAsString(String variable, String[][] value);

    void putBooleanVectorAsString(String variable, boolean[] value);

    void putBooleanMatrixAsString(String variable, boolean[][] value);

    void putByteVectorAsString(String variable, byte[] value);

    void putByteMatrixAsString(String variable, byte[][] value);

    void putShortVectorAsString(String variable, short[] value);

    void putShortMatrixAsString(String variable, short[][] value);

    void putIntegerVectorAsString(String variable, int[] value);

    void putIntegerMatrixAsString(String variable, int[][] value);

    void putLongVectorAsString(String variable, long[] value);

    void putLongMatrixAsString(String variable, long[][] value);

    void putFloatVectorAsString(String variable, float[] value);

    void putFloatMatrixAsString(String variable, float[][] value);

    void putDoubleVectorAsString(String variable, double[] value);

    void putDoubleMatrixAsString(String variable, double[][] value);

}
