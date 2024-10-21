package de.invesdwin.scripting.julia.runtime.contract.callback;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskJulia;
import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.Arrays;

@NotThreadSafe
public class ParametersAndReturnsTestLong {

    private final IScriptTaskRunnerJulia runner;

    public ParametersAndReturnsTestLong(final IScriptTaskRunnerJulia runner) {
        this.runner = runner;
    }

    public void testLong() {
        new AScriptTaskJulia<Void>() {

            @Override
            public IScriptTaskCallback getCallback() {
                final ParametersAndReturnsTestLongCallback callback = new ParametersAndReturnsTestLongCallback();
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestLong.class.getSimpleName() + ".jl",
                        ParametersAndReturnsTestLong.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

    public static class ParametersAndReturnsTestLongCallback {

        //putLong
        private final long putLong;

        //putLongVector
        private final long[] putLongVector;

        //putLongVectorAsList
        private final List<Long> putLongVectorAsList;

        //putLongMatrix
        private final long[][] putLongMatrix;

        //putLongMatrixAsList
        private final List<List<Long>> putLongMatrixAsList;

        public ParametersAndReturnsTestLongCallback() {
            //putLong
            this.putLong = 123;

            //putLongVector
            this.putLongVector = new long[3];
            for (int i = 0; i < putLongVector.length; i++) {
                putLongVector[i] = Long.parseLong((i + 1) + "" + (i + 1));
            }

            //putLongVectorAsList
            this.putLongVectorAsList = Arrays.asList(Arrays.toObject(putLongVector));

            //putLongMatrix
            this.putLongMatrix = new long[4][];
            for (int row = 0; row < putLongMatrix.length; row++) {
                final long[] vector = new long[3];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = Long.parseLong((row + 1) + "" + (col + 1));
                }
                putLongMatrix[row] = vector;
            }

            //putLongMatrixAsList
            this.putLongMatrixAsList = new ArrayList<List<Long>>(putLongMatrix.length);
            for (final long[] vector : putLongMatrix) {
                putLongMatrixAsList.add(Arrays.asList(Arrays.toObject(vector)));
            }
        }

        public long getLong() {
            return putLong;
        }

        public void setLong(final long putLong) {
            Assertions.assertThat(this.putLong).isEqualTo(putLong);
        }

        public long[] getLongVector() {
            return putLongVector;
        }

        public void setLongVector(final long[] putLongVector) {
            Assertions.assertThat(this.putLongVector).isEqualTo(putLongVector);
        }

        public List<Long> getLongVectorAsList() {
            return putLongVectorAsList;
        }

        public void setLongVectorAsList(final List<Long> putLongVectorAsList) {
            Assertions.assertThat(this.putLongVectorAsList).isEqualTo(putLongVectorAsList);
        }

        public long[][] getLongMatrix() {
            return putLongMatrix;
        }

        public void setLongMatrix(final long[][] putLongMatrix) {
            Assertions.assertThat(this.putLongMatrix).isEqualTo(putLongMatrix);
        }

        public List<List<Long>> getLongMatrixAsList() {
            return putLongMatrixAsList;
        }

        public void setLongMatrixAsList(final List<List<Long>> putLongMatrixAsList) {
            Assertions.assertThat(this.putLongMatrixAsList).isEqualTo(putLongMatrixAsList);
        }

    }

}
