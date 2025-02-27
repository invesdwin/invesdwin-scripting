package de.invesdwin.scripting.runtime.mvel.tests.callback;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.runtime.mvel.AScriptTaskMvel;
import de.invesdwin.scripting.runtime.mvel.IScriptTaskRunnerMvel;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.Arrays;

@NotThreadSafe
public class ParametersAndReturnsTestDouble {

    private final IScriptTaskRunnerMvel runner;

    public ParametersAndReturnsTestDouble(final IScriptTaskRunnerMvel runner) {
        this.runner = runner;
    }

    public void testDouble() {
        new AScriptTaskMvel<Void>() {

            @Override
            public IScriptTaskCallback getCallback() {
                final ParametersAndReturnsTestDoubleCallback callback = new ParametersAndReturnsTestDoubleCallback();
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestDouble.class.getSimpleName() + ".mvel",
                        ParametersAndReturnsTestDouble.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

    public static class ParametersAndReturnsTestDoubleCallback {

        //putDouble
        private final double putDouble;

        //putDoubleVector
        private final double[] putDoubleVector;

        //putDoubleVectorAsList
        private final List<Double> putDoubleVectorAsList;

        //putDoubleMatrix
        private final double[][] putDoubleMatrix;

        //putDoubleMatrixAsList
        private final List<List<Double>> putDoubleMatrixAsList;

        public ParametersAndReturnsTestDoubleCallback() {
            //putDouble
            this.putDouble = 123.123D;

            //putDoubleVector
            this.putDoubleVector = new double[3];
            for (int i = 0; i < putDoubleVector.length; i++) {
                putDoubleVector[i] = Double.parseDouble((i + 1) + "." + (i + 1));
            }

            //putDoubleVectorAsList
            this.putDoubleVectorAsList = Arrays.asList(Arrays.toObject(putDoubleVector));

            //putDoubleMatrix
            this.putDoubleMatrix = new double[4][];
            for (int row = 0; row < putDoubleMatrix.length; row++) {
                final double[] vector = new double[3];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = Double.parseDouble((row + 1) + "." + (col + 1));
                }
                putDoubleMatrix[row] = vector;
            }

            //putDoubleMatrixAsList
            this.putDoubleMatrixAsList = new ArrayList<List<Double>>(putDoubleMatrix.length);
            for (final double[] vector : putDoubleMatrix) {
                putDoubleMatrixAsList.add(Arrays.asList(Arrays.toObject(vector)));
            }
        }

        public double getDouble() {
            return putDouble;
        }

        public void setDouble(final double putDouble) {
            Assertions.assertThat(this.putDouble).isEqualTo(putDouble);
        }

        public double[] getDoubleVector() {
            return putDoubleVector;
        }

        public void setDoubleVector(final double[] putDoubleVector) {
            Assertions.assertThat(this.putDoubleVector).isEqualTo(putDoubleVector);
        }

        public List<Double> getDoubleVectorAsList() {
            return putDoubleVectorAsList;
        }

        public void setDoubleVectorAsList(final List<Double> putDoubleVectorAsList) {
            Assertions.assertThat(this.putDoubleVectorAsList).isEqualTo(putDoubleVectorAsList);
        }

        public double[][] getDoubleMatrix() {
            return putDoubleMatrix;
        }

        public void setDoubleMatrix(final double[][] putDoubleMatrix) {
            Assertions.assertThat(this.putDoubleMatrix).isEqualTo(putDoubleMatrix);
        }

        public List<List<Double>> getDoubleMatrixAsList() {
            return putDoubleMatrixAsList;
        }

        public void setDoubleMatrixAsList(final List<List<Double>> putDoubleMatrixAsList) {
            Assertions.assertThat(this.putDoubleMatrixAsList).isEqualTo(putDoubleMatrixAsList);
        }

    }

}
