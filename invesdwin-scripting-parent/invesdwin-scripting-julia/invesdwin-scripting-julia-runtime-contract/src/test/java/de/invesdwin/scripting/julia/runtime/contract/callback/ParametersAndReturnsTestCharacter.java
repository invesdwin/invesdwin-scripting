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
import de.invesdwin.util.math.Characters;

@NotThreadSafe
public class ParametersAndReturnsTestCharacter {

    private final IScriptTaskRunnerJulia runner;

    public ParametersAndReturnsTestCharacter(final IScriptTaskRunnerJulia runner) {
        this.runner = runner;
    }

    public void testCharacter() {
        new AScriptTaskJulia<Void>() {

            @Override
            public IScriptTaskCallback getCallback() {
                final ParametersAndReturnsTestCharacterCallback callback = new ParametersAndReturnsTestCharacterCallback();
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(ParametersAndReturnsTestCharacter.class.getSimpleName() + ".jl",
                        ParametersAndReturnsTestCharacter.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

    public static class ParametersAndReturnsTestCharacterCallback {

        //putCharacter
        private final char putCharacter;

        //putCharacterVector
        private final char[] putCharacterVector;

        //putCharacterVectorAsList
        private final List<Character> putCharacterVectorAsList;

        //putCharacterMatrix
        private final char[][] putCharacterMatrix;

        //putCharacterMatrixAsList
        private final List<List<Character>> putCharacterMatrixAsList;

        public ParametersAndReturnsTestCharacterCallback() {
            //putCharacter
            this.putCharacter = 'a';

            //putCharacterVector
            this.putCharacterVector = new char[3];
            for (int i = 0; i < putCharacterVector.length; i++) {
                putCharacterVector[i] = Characters.checkedCast('A' + i);
            }

            //putCharacterVectorAsList
            this.putCharacterVectorAsList = Characters.asList(putCharacterVector);

            //putCharacterMatrix
            this.putCharacterMatrix = new char[4][];
            for (int i = 0; i < putCharacterMatrix.length; i++) {
                final char[] vector = new char[3];
                for (int j = 0; j < vector.length; j++) {
                    vector[j] = Characters.checkedCast('A' + i + j);
                }
                putCharacterMatrix[i] = vector;
            }

            //putCharacterMatrixAsList
            this.putCharacterMatrixAsList = new ArrayList<List<Character>>(putCharacterMatrix.length);
            for (final char[] vector : putCharacterMatrix) {
                putCharacterMatrixAsList.add(Characters.asList(vector));
            }
        }

        public char getCharacter() {
            return putCharacter;
        }

        public void setCharacter(final char putCharacter) {
            Assertions.assertThat(this.putCharacter).isEqualTo(putCharacter);
        }

        public char[] getCharacterVector() {
            return putCharacterVector;
        }

        public void setCharacterVector(final char[] putCharacterVector) {
            Assertions.assertThat(this.putCharacterVector).isEqualTo(putCharacterVector);
        }

        public List<Character> getCharacterVectorAsList() {
            return putCharacterVectorAsList;
        }

        public void setCharacterVectorAsList(final List<Character> putCharacterVectorAsList) {
            Assertions.assertThat(this.putCharacterVectorAsList).isEqualTo(putCharacterVectorAsList);
        }

        public char[][] getCharacterMatrix() {
            return putCharacterMatrix;
        }

        public void setCharacterMatrix(final char[][] putCharacterMatrix) {
            Assertions.assertThat(this.putCharacterMatrix).isEqualTo(putCharacterMatrix);
        }

        public List<List<Character>> getCharacterMatrixAsList() {
            return putCharacterMatrixAsList;
        }

        public void setCharacterMatrixAsList(final List<List<Character>> putCharacterMatrixAsList) {
            Assertions.assertThat(this.putCharacterMatrixAsList).isEqualTo(putCharacterMatrixAsList);
        }

    }

}
