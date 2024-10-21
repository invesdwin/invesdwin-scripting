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
public class InputsAndResultsTestEmptyMatrixValue {

    private final IScriptTaskRunnerHaskell runner;

    public InputsAndResultsTestEmptyMatrixValue(final IScriptTaskRunnerHaskell runner) {
        this.runner = runner;
    }

    public void testEmptyMatrixValue() {
        final boolean[][] putBooleanMatrix = new boolean[2][];
        for (int i = 0; i < putBooleanMatrix.length; i++) {
            putBooleanMatrix[i] = new boolean[0];
        }
        final List<List<Boolean>> putBooleanMatrixAsList = Booleans.asListMatrix(putBooleanMatrix);

        final byte[][] putByteMatrix = new byte[2][];
        for (int i = 0; i < putByteMatrix.length; i++) {
            putByteMatrix[i] = new byte[0];
        }
        final List<List<Byte>> putByteMatrixAsList = Bytes.asListMatrix(putByteMatrix);

        final char[][] putCharacterMatrix = new char[2][];
        for (int i = 0; i < putCharacterMatrix.length; i++) {
            putCharacterMatrix[i] = new char[0];
        }
        final List<List<Character>> putCharacterMatrixAsList = Characters.asListMatrix(putCharacterMatrix);

        final Decimal[][] putDecimalMatrix = new Decimal[2][];
        for (int i = 0; i < putDecimalMatrix.length; i++) {
            putDecimalMatrix[i] = new Decimal[0];
        }
        final List<List<Decimal>> putDecimalMatrixAsList = Decimal.asListMatrix(putDecimalMatrix);

        final double[][] putDoubleMatrix = new double[2][];
        for (int i = 0; i < putDoubleMatrix.length; i++) {
            putDoubleMatrix[i] = new double[0];
        }
        final List<List<Double>> putDoubleMatrixAsList = Doubles.asListMatrix(putDoubleMatrix);

        final float[][] putFloatMatrix = new float[2][];
        for (int i = 0; i < putFloatMatrix.length; i++) {
            putFloatMatrix[i] = new float[0];
        }
        final List<List<Float>> putFloatMatrixAsList = Floats.asListMatrix(putFloatMatrix);

        final int[][] putIntegerMatrix = new int[2][];
        for (int i = 0; i < putIntegerMatrix.length; i++) {
            putIntegerMatrix[i] = new int[0];
        }
        final List<List<Integer>> putIntegerMatrixAsList = Integers.asListMatrix(putIntegerMatrix);

        final long[][] putLongMatrix = new long[2][];
        for (int i = 0; i < putLongMatrix.length; i++) {
            putLongMatrix[i] = new long[0];
        }
        final List<List<Long>> putLongMatrixAsList = Longs.asListMatrix(putLongMatrix);

        final Percent[][] putPercentMatrix = new Percent[2][];
        for (int i = 0; i < putPercentMatrix.length; i++) {
            putPercentMatrix[i] = new Percent[0];
        }
        final List<List<Percent>> putPercentMatrixAsList = Percent.asListMatrix(putPercentMatrix);

        final short[][] putShortMatrix = new short[2][];
        for (int i = 0; i < putShortMatrix.length; i++) {
            putShortMatrix[i] = new short[0];
        }
        final List<List<Short>> putShortMatrixAsList = Shorts.asListMatrix(putShortMatrix);

        final String[][] putStringMatrix = new String[2][];
        for (int i = 0; i < putStringMatrix.length; i++) {
            putStringMatrix[i] = new String[0];
        }
        final List<List<String>> putStringMatrixAsList = Strings.asListMatrix(putStringMatrix);

        new AScriptTaskHaskell<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
                inputs.putBooleanMatrix("putBooleanMatrix", putBooleanMatrix);
                inputs.putBooleanMatrixAsList("putBooleanMatrixAsList", putBooleanMatrixAsList);

                inputs.putByteMatrix("putByteMatrix", putByteMatrix);
                inputs.putByteMatrixAsList("putByteMatrixAsList", putByteMatrixAsList);

                inputs.putCharacterMatrix("putCharacterMatrix", putCharacterMatrix);
                inputs.putCharacterMatrixAsList("putCharacterMatrixAsList", putCharacterMatrixAsList);

                inputs.putDecimalMatrix("putDecimalMatrix", putDecimalMatrix);
                inputs.putDecimalMatrixAsList("putDecimalMatrixAsList", putDecimalMatrixAsList);

                inputs.putDoubleMatrix("putDoubleMatrix", putDoubleMatrix);
                inputs.putDoubleMatrixAsList("putDoubleMatrixAsList", putDoubleMatrixAsList);

                inputs.putFloatMatrix("putFloatMatrix", putFloatMatrix);
                inputs.putFloatMatrixAsList("putFloatMatrixAsList", putFloatMatrixAsList);

                inputs.putIntegerMatrix("putIntegerMatrix", putIntegerMatrix);
                inputs.putIntegerMatrixAsList("putIntegerMatrixAsList", putIntegerMatrixAsList);

                inputs.putLongMatrix("putLongMatrix", putLongMatrix);
                inputs.putLongMatrixAsList("putLongMatrixAsList", putLongMatrixAsList);

                inputs.putDecimalMatrix("putPercentMatrix", putPercentMatrix);
                inputs.putDecimalMatrixAsList("putPercentMatrixAsList", putPercentMatrixAsList);

                inputs.putShortMatrix("putShortMatrix", putShortMatrix);
                inputs.putShortMatrixAsList("putShortMatrixAsList", putShortMatrixAsList);

                inputs.putStringMatrix("putStringMatrix", putStringMatrix);
                inputs.putStringMatrixAsList("putStringMatrixAsList", putStringMatrixAsList);
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(InputsAndResultsTestEmptyMatrixValue.class.getSimpleName() + ".hs",
                        InputsAndResultsTestEmptyMatrixValue.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                final IScriptTaskResultsHaskell cResults = (IScriptTaskResultsHaskell) results;
                //                Assertions.checkEquals(putBooleanMatrix, results.getBooleanMatrix("getBooleanMatrix"));
                assertEmptyMatrixValue(cResults, "getBooleanMatrix");
                //                Assertions.checkEquals(putBooleanMatrixAsList,
                //                        results.getBooleanMatrixAsList("getBooleanMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getBooleanMatrixAsList");
                //
                //                Assertions.checkEquals(putByteMatrix, results.getByteMatrix("getByteMatrix"));
                assertEmptyMatrixValue(cResults, "getByteMatrix");
                //                Assertions.checkEquals(putByteMatrixAsList, results.getByteMatrixAsList("getByteMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getByteMatrixAsList");
                //
                //                Assertions.checkEquals(putCharacterMatrix, results.getCharacterMatrix("getCharacterMatrix"));
                assertEmptyMatrixValue(cResults, "getCharacterMatrix");
                //                Assertions.checkEquals(putCharacterMatrixAsList,
                //                        results.getCharacterMatrixAsList("getCharacterMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getCharacterMatrixAsList");
                //
                //                Assertions.checkEquals(putDecimalMatrix, results.getDecimalMatrix("getDecimalMatrix"));
                assertEmptyMatrixValue(cResults, "getDecimalMatrix");
                //                Assertions.checkEquals(putDecimalMatrixAsList,
                //                        results.getDecimalMatrixAsList("getDecimalMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getDecimalMatrixAsList");
                //
                //                Assertions.checkEquals(putDoubleMatrix, results.getDoubleMatrix("getDoubleMatrix"));
                assertEmptyMatrixValue(cResults, "getDoubleMatrix");
                //                Assertions.checkEquals(putDoubleMatrixAsList, results.getDoubleMatrixAsList("getDoubleMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getDoubleMatrixAsList");
                //
                //                Assertions.checkEquals(putFloatMatrix, results.getFloatMatrix("getFloatMatrix"));
                assertEmptyMatrixValue(cResults, "getFloatMatrix");
                //                Assertions.checkEquals(putFloatMatrixAsList, results.getFloatMatrixAsList("getFloatMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getFloatMatrixAsList");
                //
                //                Assertions.checkEquals(putIntegerMatrix, results.getIntegerMatrix("getIntegerMatrix"));
                assertEmptyMatrixValue(cResults, "getIntegerMatrix");
                //                Assertions.checkEquals(putIntegerMatrixAsList,
                //                        results.getIntegerMatrixAsList("getIntegerMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getIntegerMatrixAsList");
                //
                //                Assertions.checkEquals(putLongMatrix, results.getLongMatrix("getLongMatrix"));
                assertEmptyMatrixValue(cResults, "getLongMatrix");
                //                Assertions.checkEquals(putLongMatrixAsList, results.getLongMatrixAsList("getLongMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getLongMatrixAsList");
                //
                //                Assertions.checkEquals(putPercentMatrix,
                //                        results.getDecimalMatrix("getPercentMatrix", Percent.ZERO_PERCENT));
                assertEmptyMatrixValue(cResults, "getPercentMatrix");
                //                Assertions.checkEquals(putPercentMatrixAsList,
                //                        results.getDecimalMatrixAsList("getPercentMatrixAsList", Percent.ZERO_PERCENT));
                assertEmptyMatrixValue(cResults, "getPercentMatrixAsList");
                //
                //                Assertions.checkEquals(putShortMatrix, results.getShortMatrix("getShortMatrix"));
                assertEmptyMatrixValue(cResults, "getShortMatrix");
                //                Assertions.checkEquals(putShortMatrixAsList, results.getShortMatrixAsList("getShortMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getShortMatrixAsList");
                //
                //                Assertions.checkEquals(putStringMatrix, results.getStringMatrix("getStringMatrix"));
                assertEmptyMatrixValue(cResults, "getStringMatrix");
                //                Assertions.checkEquals(putStringMatrixAsList, results.getStringMatrixAsList("getStringMatrixAsList"));
                assertEmptyMatrixValue(cResults, "getStringMatrixAsList");
                return null;
            }

            private void assertEmptyMatrixValue(final IScriptTaskResultsHaskell cResults, final String variable) {
                Assertions.checkEquals(2, cResults.length(variable));
                Assertions.checkTrue(cResults.isEmpty(variable + "!!0"));
                Assertions.checkTrue(cResults.isEmpty(variable + "!!1"));
            }
        }.run(runner);
    }

}
