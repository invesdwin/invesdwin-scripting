package de.invesdwin.scripting.haskell.runtime.contract;

import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;
import de.invesdwin.util.math.Bytes;
import de.invesdwin.util.math.Characters;
import de.invesdwin.util.math.Doubles;
import de.invesdwin.util.math.Floats;
import de.invesdwin.util.math.Integers;
import de.invesdwin.util.math.Longs;
import de.invesdwin.util.math.Shorts;
import de.invesdwin.util.math.decimal.Decimal;
import de.invesdwin.util.math.decimal.scaled.Percent;

@NotThreadSafe
public class InputsAndResultsTestEmpty {

    private final IScriptTaskRunnerHaskell runner;

    public InputsAndResultsTestEmpty(final IScriptTaskRunnerHaskell runner) {
        this.runner = runner;
    }

    public void testEmpty() {
        final boolean[] putBooleanVector = new boolean[0];
        final List<Boolean> putBooleanVectorAsList = Booleans.asListVector(putBooleanVector);
        final boolean[][] putBooleanMatrix = new boolean[0][];
        final List<List<Boolean>> putBooleanMatrixAsList = Booleans.asListMatrix(putBooleanMatrix);

        final byte[] putByteVector = new byte[0];
        final List<Byte> putByteVectorAsList = Bytes.asListVector(putByteVector);
        final byte[][] putByteMatrix = new byte[0][];
        final List<List<Byte>> putByteMatrixAsList = Bytes.asListMatrix(putByteMatrix);

        final char[] putCharacterVector = new char[0];
        final List<Character> putCharacterVectorAsList = Characters.asListVector(putCharacterVector);
        final char[][] putCharacterMatrix = new char[0][];
        final List<List<Character>> putCharacterMatrixAsList = Characters.asListMatrix(putCharacterMatrix);

        final Decimal[] putDecimalVector = new Decimal[0];
        final List<Decimal> putDecimalVectorAsList = Decimal.asListVector(putDecimalVector);
        final Decimal[][] putDecimalMatrix = new Decimal[0][];
        final List<List<Decimal>> putDecimalMatrixAsList = Decimal.asListMatrix(putDecimalMatrix);

        final double[] putDoubleVector = new double[0];
        final List<Double> putDoubleVectorAsList = Doubles.asListVector(putDoubleVector);
        final double[][] putDoubleMatrix = new double[0][];
        final List<List<Double>> putDoubleMatrixAsList = Doubles.asListMatrix(putDoubleMatrix);

        final float[] putFloatVector = new float[0];
        final List<Float> putFloatVectorAsList = Floats.asListVector(putFloatVector);
        final float[][] putFloatMatrix = new float[0][];
        final List<List<Float>> putFloatMatrixAsList = Floats.asListMatrix(putFloatMatrix);

        final int[] putIntegerVector = new int[0];
        final List<Integer> putIntegerVectorAsList = Integers.asListVector(putIntegerVector);
        final int[][] putIntegerMatrix = new int[0][];
        final List<List<Integer>> putIntegerMatrixAsList = Integers.asListMatrix(putIntegerMatrix);

        final long[] putLongVector = new long[0];
        final List<Long> putLongVectorAsList = Longs.asListVector(putLongVector);
        final long[][] putLongMatrix = new long[0][];
        final List<List<Long>> putLongMatrixAsList = Longs.asListMatrix(putLongMatrix);

        final Percent[] putPercentVector = new Percent[0];
        final List<Percent> putPercentVectorAsList = Percent.asListVector(putPercentVector);
        final Percent[][] putPercentMatrix = new Percent[0][];
        final List<List<Percent>> putPercentMatrixAsList = Percent.asListMatrix(putPercentMatrix);

        final short[] putShortVector = new short[0];
        final List<Short> putShortVectorAsList = Shorts.asListVector(putShortVector);
        final short[][] putShortMatrix = new short[0][];
        final List<List<Short>> putShortMatrixAsList = Shorts.asListMatrix(putShortMatrix);

        final String[] putStringVector = new String[0];
        final List<String> putStringVectorAsList = Strings.asListVector(putStringVector);
        final String[][] putStringMatrix = new String[0][];
        final List<List<String>> putStringMatrixAsList = Strings.asListMatrix(putStringMatrix);

        new AScriptTaskHaskell<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
                inputs.putBooleanVector("putBooleanVector", putBooleanVector);
                inputs.putBooleanVectorAsList("putBooleanVectorAsList", putBooleanVectorAsList);
                inputs.putBooleanMatrix("putBooleanMatrix", putBooleanMatrix);
                inputs.putBooleanMatrixAsList("putBooleanMatrixAsList", putBooleanMatrixAsList);

                inputs.putByteVector("putByteVector", putByteVector);
                inputs.putByteVectorAsList("putByteVectorAsList", putByteVectorAsList);
                inputs.putByteMatrix("putByteMatrix", putByteMatrix);
                inputs.putByteMatrixAsList("putByteMatrixAsList", putByteMatrixAsList);

                inputs.putCharacterVector("putCharacterVector", putCharacterVector);
                inputs.putCharacterVectorAsList("putCharacterVectorAsList", putCharacterVectorAsList);
                inputs.putCharacterMatrix("putCharacterMatrix", putCharacterMatrix);
                inputs.putCharacterMatrixAsList("putCharacterMatrixAsList", putCharacterMatrixAsList);

                inputs.putDecimalVector("putDecimalVector", putDecimalVector);
                inputs.putDecimalVectorAsList("putDecimalVectorAsList", putDecimalVectorAsList);
                inputs.putDecimalMatrix("putDecimalMatrix", putDecimalMatrix);
                inputs.putDecimalMatrixAsList("putDecimalMatrixAsList", putDecimalMatrixAsList);

                inputs.putDoubleVector("putDoubleVector", putDoubleVector);
                inputs.putDoubleVectorAsList("putDoubleVectorAsList", putDoubleVectorAsList);
                inputs.putDoubleMatrix("putDoubleMatrix", putDoubleMatrix);
                inputs.putDoubleMatrixAsList("putDoubleMatrixAsList", putDoubleMatrixAsList);

                inputs.putFloatVector("putFloatVector", putFloatVector);
                inputs.putFloatVectorAsList("putFloatVectorAsList", putFloatVectorAsList);
                inputs.putFloatMatrix("putFloatMatrix", putFloatMatrix);
                inputs.putFloatMatrixAsList("putFloatMatrixAsList", putFloatMatrixAsList);

                inputs.putIntegerVector("putIntegerVector", putIntegerVector);
                inputs.putIntegerVectorAsList("putIntegerVectorAsList", putIntegerVectorAsList);
                inputs.putIntegerMatrix("putIntegerMatrix", putIntegerMatrix);
                inputs.putIntegerMatrixAsList("putIntegerMatrixAsList", putIntegerMatrixAsList);

                inputs.putLongVector("putLongVector", putLongVector);
                inputs.putLongVectorAsList("putLongVectorAsList", putLongVectorAsList);
                inputs.putLongMatrix("putLongMatrix", putLongMatrix);
                inputs.putLongMatrixAsList("putLongMatrixAsList", putLongMatrixAsList);

                inputs.putDecimalVector("putPercentVector", putPercentVector);
                inputs.putDecimalVectorAsList("putPercentVectorAsList", putPercentVectorAsList);
                inputs.putDecimalMatrix("putPercentMatrix", putPercentMatrix);
                inputs.putDecimalMatrixAsList("putPercentMatrixAsList", putPercentMatrixAsList);

                inputs.putShortVector("putShortVector", putShortVector);
                inputs.putShortVectorAsList("putShortVectorAsList", putShortVectorAsList);
                inputs.putShortMatrix("putShortMatrix", putShortMatrix);
                inputs.putShortMatrixAsList("putShortMatrixAsList", putShortMatrixAsList);

                inputs.putStringVector("putStringVector", putStringVector);
                inputs.putStringVectorAsList("putStringVectorAsList", putStringVectorAsList);
                inputs.putStringMatrix("putStringMatrix", putStringMatrix);
                inputs.putStringMatrixAsList("putStringMatrixAsList", putStringMatrixAsList);
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(InputsAndResultsTestEmpty.class.getSimpleName() + ".hs",
                        InputsAndResultsTestEmpty.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                final IScriptTaskResultsHaskell cResults = (IScriptTaskResultsHaskell) results;
                //Haskell/Frege toJSON does not support encoding empty arrays
                //                Assertions.checkEquals(putBooleanVector, results.getBooleanVector("getBooleanVector"));
                Assertions.checkTrue(cResults.isEmpty("getBooleanVector"));
                //                Assertions.checkEquals(putBooleanVectorAsList,
                //                        results.getBooleanVectorAsList("getBooleanVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getBooleanVectorAsList"));
                //                Assertions.checkEquals(putBooleanMatrix, results.getBooleanMatrix("getBooleanMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getBooleanVectorAsList"));
                //                Assertions.checkEquals(putBooleanMatrixAsList,
                //                        results.getBooleanMatrixAsList("getBooleanMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getBooleanVectorAsList"));
                //
                //                Assertions.checkEquals(putByteVector, results.getByteVector("getByteVector"));
                Assertions.checkTrue(cResults.isEmpty("getByteVector"));
                //                Assertions.checkEquals(putByteVectorAsList, results.getByteVectorAsList("getByteVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getByteVectorAsList"));
                //                Assertions.checkEquals(putByteMatrix, results.getByteMatrix("getByteMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getByteMatrix"));
                //                Assertions.checkEquals(putByteMatrixAsList, results.getByteMatrixAsList("getByteMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getByteMatrixAsList"));
                //
                //                Assertions.checkEquals(putCharacterVector, results.getCharacterVector("getCharacterVector"));
                Assertions.checkTrue(cResults.isEmpty("getCharacterVector"));
                //                Assertions.checkEquals(putCharacterVectorAsList,
                //                        results.getCharacterVectorAsList("getCharacterVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getCharacterVectorAsList"));
                //                Assertions.checkEquals(putCharacterMatrix, results.getCharacterMatrix("getCharacterMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getCharacterMatrix"));
                //                Assertions.checkEquals(putCharacterMatrixAsList,
                //                        results.getCharacterMatrixAsList("getCharacterMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getCharacterMatrixAsList"));
                //
                //                Assertions.checkEquals(putDecimalVector, results.getDecimalVector("getDecimalVector"));
                Assertions.checkTrue(cResults.isEmpty("getDecimalVector"));
                //                Assertions.checkEquals(putDecimalVectorAsList,
                //                        results.getDecimalVectorAsList("getDecimalVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getDecimalVectorAsList"));
                //                Assertions.checkEquals(putDecimalMatrix, results.getDecimalMatrix("getDecimalMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getDecimalMatrix"));
                //                Assertions.checkEquals(putDecimalMatrixAsList,
                //                        results.getDecimalMatrixAsList("getDecimalMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getDecimalMatrixAsList"));
                //
                //                Assertions.checkEquals(putDoubleVector, results.getDoubleVector("getDoubleVector"));
                Assertions.checkTrue(cResults.isEmpty("getDoubleVector"));
                //                Assertions.checkEquals(putDoubleVectorAsList, results.getDoubleVectorAsList("getDoubleVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getDoubleVectorAsList"));
                //                Assertions.checkEquals(putDoubleMatrix, results.getDoubleMatrix("getDoubleMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getDoubleMatrix"));
                //                Assertions.checkEquals(putDoubleMatrixAsList, results.getDoubleMatrixAsList("getDoubleMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getDoubleMatrixAsList"));
                //
                //                Assertions.checkEquals(putFloatVector, results.getFloatVector("getFloatVector"));
                Assertions.checkTrue(cResults.isEmpty("getFloatVector"));
                //                Assertions.checkEquals(putFloatVectorAsList, results.getFloatVectorAsList("getFloatVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getFloatVectorAsList"));
                //                Assertions.checkEquals(putFloatMatrix, results.getFloatMatrix("getFloatMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getFloatMatrix"));
                //                Assertions.checkEquals(putFloatMatrixAsList, results.getFloatMatrixAsList("getFloatMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getFloatMatrixAsList"));
                //
                //                Assertions.checkEquals(putIntegerVector, results.getIntegerVector("getIntegerVector"));
                Assertions.checkTrue(cResults.isEmpty("getIntegerVector"));
                //                Assertions.checkEquals(putIntegerVectorAsList,
                //                        results.getIntegerVectorAsList("getIntegerVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getIntegerVectorAsList"));
                //                Assertions.checkEquals(putIntegerMatrix, results.getIntegerMatrix("getIntegerMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getIntegerMatrix"));
                //                Assertions.checkEquals(putIntegerMatrixAsList,
                //                        results.getIntegerMatrixAsList("getIntegerMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getIntegerMatrixAsList"));
                //
                //                Assertions.checkEquals(putLongVector, results.getLongVector("getLongVector"));
                Assertions.checkTrue(cResults.isEmpty("getLongVector"));
                //                Assertions.checkEquals(putLongVectorAsList, results.getLongVectorAsList("getLongVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getLongVectorAsList"));
                //                Assertions.checkEquals(putLongMatrix, results.getLongMatrix("getLongMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getLongMatrix"));
                //                Assertions.checkEquals(putLongMatrixAsList, results.getLongMatrixAsList("getLongMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("putLongMatrixAsList"));
                //
                //                Assertions.checkEquals(putPercentVector,
                //                        results.getDecimalVector("getPercentVector", Percent.ZERO_PERCENT));
                Assertions.checkTrue(cResults.isEmpty("getPercentVector"));
                //                Assertions.checkEquals(putPercentVectorAsList,
                //                        results.getDecimalVectorAsList("getPercentVectorAsList", Percent.ZERO_PERCENT));
                Assertions.checkTrue(cResults.isEmpty("getPercentVectorAsList"));
                //                Assertions.checkEquals(putPercentMatrix,
                //                        results.getDecimalMatrix("getPercentMatrix", Percent.ZERO_PERCENT));
                Assertions.checkTrue(cResults.isEmpty("getPercentMatrix"));
                //                Assertions.checkEquals(putPercentMatrixAsList,
                //                        results.getDecimalMatrixAsList("getPercentMatrixAsList", Percent.ZERO_PERCENT));
                Assertions.checkTrue(cResults.isEmpty("getPercentMatrixAsList"));
                //
                //                Assertions.checkEquals(putShortVector, results.getShortVector("getShortVector"));
                Assertions.checkTrue(cResults.isEmpty("getShortVector"));
                //                Assertions.checkEquals(putShortVectorAsList, results.getShortVectorAsList("getShortVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getShortVectorAsList"));
                //                Assertions.checkEquals(putShortMatrix, results.getShortMatrix("getShortMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getShortMatrix"));
                //                Assertions.checkEquals(putShortMatrixAsList, results.getShortMatrixAsList("getShortMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getShortMatrixAsList"));
                //
                //                Assertions.checkEquals(putStringVector, results.getStringVector("getStringVector"));
                Assertions.checkTrue(cResults.isEmpty("getStringVector"));
                //                Assertions.checkEquals(putStringVectorAsList, results.getStringVectorAsList("getStringVectorAsList"));
                Assertions.checkTrue(cResults.isEmpty("getStringVectorAsList"));
                //                Assertions.checkEquals(putStringMatrix, results.getStringMatrix("getStringMatrix"));
                Assertions.checkTrue(cResults.isEmpty("getStringMatrix"));
                //                Assertions.checkEquals(putStringMatrixAsList, results.getStringMatrixAsList("getStringMatrixAsList"));
                Assertions.checkTrue(cResults.isEmpty("getStringMatrixAsList"));
                return null;
            }
        }.run(runner);
    }

}
