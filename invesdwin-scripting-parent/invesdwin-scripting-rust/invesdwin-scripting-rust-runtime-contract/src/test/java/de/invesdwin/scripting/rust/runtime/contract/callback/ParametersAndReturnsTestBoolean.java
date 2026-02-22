package de.invesdwin.scripting.rust.runtime.contract.callback;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskRust;
import de.invesdwin.scripting.rust.runtime.contract.IScriptTaskRunnerRust;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.Arrays;

@NotThreadSafe
public class ParametersAndReturnsTestBoolean {

    private final IScriptTaskRunnerRust runner;

    public ParametersAndReturnsTestBoolean(final IScriptTaskRunnerRust runner) {
        this.runner = runner;
    }

    public void testBoolean() {
        new AScriptTaskRust<Void>() {
            private final ParametersAndReturnsTestBooleanCallback callback = new ParametersAndReturnsTestBooleanCallback();

            @Override
            public IScriptTaskCallback getCallback() {
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestBoolean.class.getSimpleName() + ".rs",
                        ParametersAndReturnsTestBoolean.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                //force evaluation in irust
                Assertions.checkTrue(results.getBoolean("true"));
                Assertions.assertThat(callback.setterMethodsCalled.get()).isEqualTo(5);
                return null;
            }
        }.run(runner);
    }

    public static class ParametersAndReturnsTestBooleanCallback {

        private final AtomicInteger setterMethodsCalled = new AtomicInteger();

        //putBoolean
        private final boolean putBoolean;

        //putBooleanVector
        private final boolean[] putBooleanVector;

        //putBooleanVectorAsList
        private final List<Boolean> putBooleanVectorAsList;

        //putBooleanMatrix
        private final boolean[][] putBooleanMatrix;

        //putBooleanMatrixAsList
        private final List<List<Boolean>> putBooleanMatrixAsList;

        public ParametersAndReturnsTestBooleanCallback() {
            //putBoolean
            this.putBoolean = true;

            //putBooleanVector
            this.putBooleanVector = new boolean[3];
            for (int i = 0; i < putBooleanVector.length; i++) {
                putBooleanVector[i] = i % 2 == 0;
            }

            //putBooleanVectorAsList
            this.putBooleanVectorAsList = Arrays.asList(Arrays.toObject(putBooleanVector));

            //putBooleanMatrix
            this.putBooleanMatrix = new boolean[4][];
            for (int i = 0; i < putBooleanMatrix.length; i++) {
                final boolean[] vector = new boolean[3];
                for (int j = 0; j < vector.length; j++) {
                    vector[j] = j % 2 == 0;
                }
                putBooleanMatrix[i] = vector;
            }

            //putBooleanMatrixAsList
            this.putBooleanMatrixAsList = new ArrayList<List<Boolean>>(putBooleanMatrix.length);
            for (final boolean[] vector : putBooleanMatrix) {
                putBooleanMatrixAsList.add(Arrays.asList(Arrays.toObject(vector)));
            }
        }

        public boolean getBoolean() {
            return putBoolean;
        }

        public void setBoolean(final boolean putBoolean) {
            Assertions.assertThat(this.putBoolean).isEqualTo(putBoolean);
            setterMethodsCalled.incrementAndGet();
        }

        public boolean[] getBooleanVector() {
            return putBooleanVector;
        }

        public void setBooleanVector(final boolean[] putBooleanVector) {
            Assertions.assertThat(this.putBooleanVector).isEqualTo(putBooleanVector);
            setterMethodsCalled.incrementAndGet();
        }

        public List<Boolean> getBooleanVectorAsList() {
            return putBooleanVectorAsList;
        }

        public void setBooleanVectorAsList(final List<Boolean> putBooleanVectorAsList) {
            Assertions.assertThat(this.putBooleanVectorAsList).isEqualTo(putBooleanVectorAsList);
            setterMethodsCalled.incrementAndGet();
        }

        public boolean[][] getBooleanMatrix() {
            return putBooleanMatrix;
        }

        public void setBooleanMatrix(final boolean[][] putBooleanMatrix) {
            Assertions.assertThat(this.putBooleanMatrix).isEqualTo(putBooleanMatrix);
            setterMethodsCalled.incrementAndGet();
        }

        public List<List<Boolean>> getBooleanMatrixAsList() {
            return putBooleanMatrixAsList;
        }

        public void setBooleanMatrixAsList(final List<List<Boolean>> putBooleanMatrixAsList) {
            Assertions.assertThat(this.putBooleanMatrixAsList).isEqualTo(putBooleanMatrixAsList);
            setterMethodsCalled.incrementAndGet();
        }

    }

}
