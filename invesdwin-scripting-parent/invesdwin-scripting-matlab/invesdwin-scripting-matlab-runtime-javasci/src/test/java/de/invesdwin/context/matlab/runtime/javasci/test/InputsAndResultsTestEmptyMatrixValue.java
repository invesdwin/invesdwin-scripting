package de.invesdwin.context.matlab.runtime.javasci.test;

import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.integration.script.IScriptTaskInputs;
import de.invesdwin.context.integration.script.IScriptTaskResults;
import de.invesdwin.context.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
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

    private final IScriptTaskRunnerMatlab runner;

    public InputsAndResultsTestEmptyMatrixValue(final IScriptTaskRunnerMatlab runner) {
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

        new AScriptTaskMatlab<Void>() {

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
                engine.eval(new ClassPathResource(InputsAndResultsTestEmptyMatrixValue.class.getSimpleName() + ".sce",
                        InputsAndResultsTestEmptyMatrixValue.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                Assertions.checkEquals(putBooleanMatrix[0], results.getBooleanMatrix("getBooleanMatrix"));
                Assertions.checkEquals(putBooleanMatrixAsList.get(0),
                        results.getBooleanMatrixAsList("getBooleanMatrixAsList"));

                Assertions.checkEquals(putByteMatrix[0], results.getByteMatrix("getByteMatrix"));
                Assertions.checkEquals(putByteMatrixAsList.get(0), results.getByteMatrixAsList("getByteMatrixAsList"));

                Assertions.checkEquals(putCharacterMatrix[0], results.getCharacterMatrix("getCharacterMatrix"));
                Assertions.checkEquals(putCharacterMatrixAsList.get(0),
                        results.getCharacterMatrixAsList("getCharacterMatrixAsList"));

                Assertions.checkEquals(putDecimalMatrix[0], results.getDecimalMatrix("getDecimalMatrix"));
                Assertions.checkEquals(putDecimalMatrixAsList.get(0),
                        results.getDecimalMatrixAsList("getDecimalMatrixAsList"));

                Assertions.checkEquals(putDoubleMatrix[0], results.getDoubleMatrix("getDoubleMatrix"));
                Assertions.checkEquals(putDoubleMatrixAsList.get(0),
                        results.getDoubleMatrixAsList("getDoubleMatrixAsList"));

                Assertions.checkEquals(putFloatMatrix[0], results.getFloatMatrix("getFloatMatrix"));
                Assertions.checkEquals(putFloatMatrixAsList.get(0),
                        results.getFloatMatrixAsList("getFloatMatrixAsList"));

                Assertions.checkEquals(putIntegerMatrix[0], results.getIntegerMatrix("getIntegerMatrix"));
                Assertions.checkEquals(putIntegerMatrixAsList.get(0),
                        results.getIntegerMatrixAsList("getIntegerMatrixAsList"));

                Assertions.checkEquals(putLongMatrix[0], results.getLongMatrix("getLongMatrix"));
                Assertions.checkEquals(putLongMatrixAsList.get(0), results.getLongMatrixAsList("getLongMatrixAsList"));

                Assertions.checkEquals(putPercentMatrix[0],
                        results.getDecimalMatrix("getPercentMatrix", Percent.ZERO_PERCENT));
                Assertions.checkEquals(putPercentMatrixAsList.get(0),
                        results.getDecimalMatrixAsList("getPercentMatrixAsList", Percent.ZERO_PERCENT));

                Assertions.checkEquals(putShortMatrix[0], results.getShortMatrix("getShortMatrix"));
                Assertions.checkEquals(putShortMatrixAsList.get(0),
                        results.getShortMatrixAsList("getShortMatrixAsList"));

                Assertions.checkEquals(putStringMatrix[0], results.getStringMatrix("getStringMatrix"));
                Assertions.checkEquals(putStringMatrixAsList.get(0),
                        results.getStringMatrixAsList("getStringMatrixAsList"));
                return null;
            }
        }.run(runner);
    }

}
