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
import de.invesdwin.util.lang.Objects;

@NotThreadSafe
public class ParametersAndReturnsTestString {

    private final IScriptTaskRunnerRust runner;

    public ParametersAndReturnsTestString(final IScriptTaskRunnerRust runner) {
        this.runner = runner;
    }

    public void testString() {
        new AScriptTaskRust<Void>() {
            private final ParametersAndReturnsTestStringCallback callback = new ParametersAndReturnsTestStringCallback();

            @Override
            public IScriptTaskCallback getCallback() {
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestString.class.getSimpleName() + ".rs",
                        ParametersAndReturnsTestString.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                //force evaluation in irust
                Assertions.checkTrue(results.getBoolean("true"));
                Assertions.assertThat(callback.setterMethodsCalled.get()).isEqualTo(10);
                return null;
            }
        }.run(runner);
    }

    public static class ParametersAndReturnsTestStringCallback {

        private final AtomicInteger setterMethodsCalled = new AtomicInteger();

        //putString
        private final String putString;
        private final String putStringWithNull;

        //putStringVector
        private final String[] putStringVector;
        private final String[] putStringVectorWithNull;

        //putStringVectorAsList
        private final List<String> putStringVectorAsList;
        private final List<String> putStringVectorAsListWithNull;

        //putStringMatrix
        private final String[][] putStringMatrix;
        private final String[][] putStringMatrixWithNull;

        //putStringMatrixAsList
        private final List<List<String>> putStringMatrixAsList;
        private final List<List<String>> putStringMatrixAsListWithNull;

        public ParametersAndReturnsTestStringCallback() {
            //putString
            this.putString = "asdf";
            this.putStringWithNull = null;

            //putStringVector
            this.putStringVector = new String[3];
            for (int i = 0; i < putStringVector.length; i++) {
                putStringVector[i] = i + "-" + i;
            }
            this.putStringVectorWithNull = Objects.deepClone(putStringVector);
            putStringVectorWithNull[1] = null;

            //putStringVectorAsList
            this.putStringVectorAsList = Arrays.asList(putStringVector);
            this.putStringVectorAsListWithNull = Objects.deepClone(putStringVectorAsList);
            putStringVectorAsListWithNull.set(1, null);

            //putStringMatrix
            this.putStringMatrix = new String[4][];
            for (int i = 0; i < putStringMatrix.length; i++) {
                final String[] vector = new String[3];
                for (int j = 0; j < vector.length; j++) {
                    vector[j] = i + "" + j + "-" + i + "" + j;
                }
                putStringMatrix[i] = vector;
            }
            this.putStringMatrixWithNull = Objects.deepClone(putStringMatrix);
            for (int i = 0; i < putStringMatrixWithNull[0].length; i++) {
                putStringMatrixWithNull[i][i] = null;
            }

            //putStringMatrixAsList
            this.putStringMatrixAsList = new ArrayList<List<String>>(putStringMatrix.length);
            for (final String[] vector : putStringMatrix) {
                putStringMatrixAsList.add(Arrays.asList(vector));
            }
            this.putStringMatrixAsListWithNull = Objects.deepClone(putStringMatrixAsList);
            for (int i = 0; i < putStringMatrixAsListWithNull.get(0).size(); i++) {
                putStringMatrixAsListWithNull.get(i).set(i, null);
            }
        }

        public String getString() {
            return putString;
        }

        public void setString(final String putString) {
            Assertions.assertThat(this.putString).isEqualTo(putString);
            setterMethodsCalled.incrementAndGet();
        }

        public String getStringWithNull() {
            return putStringWithNull;
        }

        public void setStringWithNull(final String putStringWithNull) {
            Assertions.assertThat(this.putStringWithNull).isEqualTo(putStringWithNull);
            setterMethodsCalled.incrementAndGet();
        }

        public String[] getStringVector() {
            return putStringVector;
        }

        public void setStringVector(final String[] putStringVector) {
            Assertions.assertThat(this.putStringVector).isEqualTo(putStringVector);
            setterMethodsCalled.incrementAndGet();
        }

        public String[] getStringVectorWithNull() {
            return putStringVectorWithNull;
        }

        public void setStringVectorWithNull(final String[] putStringVectorWithNull) {
            Assertions.assertThat(this.putStringVectorWithNull).isEqualTo(putStringVectorWithNull);
            setterMethodsCalled.incrementAndGet();
        }

        public List<String> getStringVectorAsList() {
            return putStringVectorAsList;
        }

        public void setStringVectorAsList(final List<String> putStringVectorAsList) {
            Assertions.assertThat(this.putStringVectorAsList).isEqualTo(putStringVectorAsList);
            setterMethodsCalled.incrementAndGet();
        }

        public List<String> getStringVectorAsListWithNull() {
            return putStringVectorAsListWithNull;
        }

        public void setStringVectorAsListWithNull(final List<String> putStringVectorAsListWithNull) {
            Assertions.assertThat(this.putStringVectorAsListWithNull).isEqualTo(putStringVectorAsListWithNull);
            setterMethodsCalled.incrementAndGet();
        }

        public String[][] getStringMatrix() {
            return putStringMatrix;
        }

        public void setStringMatrix(final String[][] putStringMatrix) {
            Assertions.assertThat(this.putStringMatrix).isEqualTo(putStringMatrix);
            setterMethodsCalled.incrementAndGet();
        }

        public String[][] getStringMatrixWithNull() {
            return putStringMatrixWithNull;
        }

        public void setStringMatrixWithNull(final String[][] putStringMatrixWithNull) {
            Assertions.assertThat(this.putStringMatrixWithNull).isEqualTo(putStringMatrixWithNull);
            setterMethodsCalled.incrementAndGet();
        }

        public List<List<String>> getStringMatrixAsList() {
            return putStringMatrixAsList;
        }

        public void setStringMatrixAsList(final List<List<String>> putStringMatrixAsList) {
            Assertions.assertThat(this.putStringMatrixAsList).isEqualTo(putStringMatrixAsList);
            setterMethodsCalled.incrementAndGet();
        }

        public List<List<String>> getStringMatrixAsListWithNull() {
            return putStringMatrixAsListWithNull;
        }

        public void setStringMatrixAsListWithNull(final List<List<String>> putStringMatrixAsListWithNull) {
            Assertions.assertThat(this.putStringMatrixAsListWithNull).isEqualTo(putStringMatrixAsListWithNull);
            setterMethodsCalled.incrementAndGet();
        }

    }

}
