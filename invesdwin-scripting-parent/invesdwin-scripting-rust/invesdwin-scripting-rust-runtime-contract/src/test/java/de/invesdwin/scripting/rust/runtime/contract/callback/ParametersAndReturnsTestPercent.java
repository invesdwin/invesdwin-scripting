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
import de.invesdwin.util.math.decimal.Decimal;
import de.invesdwin.util.math.decimal.scaled.Percent;
import de.invesdwin.util.math.decimal.scaled.PercentScale;

@NotThreadSafe
public class ParametersAndReturnsTestPercent {

    private final IScriptTaskRunnerRust runner;

    public ParametersAndReturnsTestPercent(final IScriptTaskRunnerRust runner) {
        this.runner = runner;
    }

    public void testPercent() {
        new AScriptTaskRust<Void>() {
            private final ParametersAndReturnsTestPercentCallback callback = new ParametersAndReturnsTestPercentCallback();

            @Override
            public IScriptTaskCallback getCallback() {
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestPercent.class.getSimpleName() + ".rs",
                        ParametersAndReturnsTestPercent.class));
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

    public static class ParametersAndReturnsTestPercentCallback {

        private final AtomicInteger setterMethodsCalled = new AtomicInteger();

        //putPercent
        private final Percent putPercent;

        //putPercentVector
        private final Percent[] putPercentVector;

        //putPercentVectorAsList
        private final List<Percent> putPercentVectorAsList;

        //putPercentMatrix
        private final Percent[][] putPercentMatrix;

        //putPercentMatrixAsList
        private final List<List<Percent>> putPercentMatrixAsList;

        public ParametersAndReturnsTestPercentCallback() {
            //putPercent
            this.putPercent = new Percent(123.123D, PercentScale.RATE);

            //putPercentVector
            this.putPercentVector = new Percent[3];
            for (int i = 0; i < putPercentVector.length; i++) {
                putPercentVector[i] = new Percent(new Decimal((i + 1) + "." + (i + 1)), PercentScale.RATE);
            }

            //putPercentVectorAsList
            this.putPercentVectorAsList = Arrays.asList(putPercentVector);

            //putPercentMatrix
            this.putPercentMatrix = new Percent[4][];
            for (int row = 0; row < putPercentMatrix.length; row++) {
                final Percent[] vector = new Percent[3];
                for (int col = 0; col < vector.length; col++) {
                    vector[col] = new Percent(new Decimal((row + 1) + "." + (col + 1)), PercentScale.RATE);
                }
                putPercentMatrix[row] = vector;
            }

            //putPercentMatrixAsList
            this.putPercentMatrixAsList = new ArrayList<List<Percent>>(putPercentMatrix.length);
            for (final Percent[] vector : putPercentMatrix) {
                putPercentMatrixAsList.add(Arrays.asList(vector));
            }
        }

        public Percent getPercent() {
            return putPercent;
        }

        public void setPercent(final Percent putPercent) {
            Assertions.assertThat(this.putPercent).isEqualTo(putPercent);
            setterMethodsCalled.incrementAndGet();
        }

        public Percent[] getPercentVector() {
            return putPercentVector;
        }

        public void setPercentVector(final Percent[] putPercentVector) {
            Assertions.assertThat(this.putPercentVector).isEqualTo(putPercentVector);
            setterMethodsCalled.incrementAndGet();
        }

        public List<Percent> getPercentVectorAsList() {
            return putPercentVectorAsList;
        }

        public void setPercentVectorAsList(final List<Percent> putPercentVectorAsList) {
            Assertions.assertThat(this.putPercentVectorAsList).isEqualTo(putPercentVectorAsList);
            setterMethodsCalled.incrementAndGet();
        }

        public Percent[][] getPercentMatrix() {
            return putPercentMatrix;
        }

        public void setPercentMatrix(final Percent[][] putPercentMatrix) {
            Assertions.assertThat(this.putPercentMatrix).isEqualTo(putPercentMatrix);
            setterMethodsCalled.incrementAndGet();
        }

        public List<List<Percent>> getPercentMatrixAsList() {
            return putPercentMatrixAsList;
        }

        public void setPercentMatrixAsList(final List<List<Percent>> putPercentMatrixAsList) {
            Assertions.assertThat(this.putPercentMatrixAsList).isEqualTo(putPercentMatrixAsList);
            setterMethodsCalled.incrementAndGet();
        }

    }

}
