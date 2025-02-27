package de.invesdwin.scripting.rust.runtime.contract;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.math.Characters;

@NotThreadSafe
public class InputsAndResultsTestCharacter {

    private final IScriptTaskRunnerRust runner;

    public InputsAndResultsTestCharacter(final IScriptTaskRunnerRust runner) {
        this.runner = runner;
    }

    public void testCharacter() {
        //putCharacter
        final char putCharacter = 'a';

        //putCharacterVector
        final char[] putCharacterVector = new char[3];
        for (int i = 0; i < putCharacterVector.length; i++) {
            putCharacterVector[i] = Characters.checkedCast('A' + i);
        }

        //putCharacterVectorAsList
        final List<Character> putCharacterVectorAsList = Characters.asList(putCharacterVector);

        //putCharacterMatrix
        final char[][] putCharacterMatrix = new char[4][];
        for (int i = 0; i < putCharacterMatrix.length; i++) {
            final char[] vector = new char[3];
            for (int j = 0; j < vector.length; j++) {
                vector[j] = Characters.checkedCast('A' + i + j);
            }
            putCharacterMatrix[i] = vector;
        }

        //putCharacterMatrixAsList
        final List<List<Character>> putCharacterMatrixAsList = new ArrayList<List<Character>>(
                putCharacterMatrix.length);
        for (final char[] vector : putCharacterMatrix) {
            putCharacterMatrixAsList.add(Characters.asList(vector));
        }

        new AScriptTaskRust<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
                inputs.putCharacter("putCharacter", putCharacter);

                inputs.putCharacterVector("putCharacterVector", putCharacterVector);

                inputs.putCharacterVectorAsList("putCharacterVectorAsList", putCharacterVectorAsList);

                inputs.putCharacterMatrix("putCharacterMatrix", putCharacterMatrix);

                inputs.putCharacterMatrixAsList("putCharacterMatrixAsList", putCharacterMatrixAsList);
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(InputsAndResultsTestCharacter.class.getSimpleName() + ".rs",
                        InputsAndResultsTestCharacter.class));
            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                //getCharacter
                final char getCharacter = results.getCharacter("getCharacter");
                Assertions.assertThat(putCharacter).isEqualTo(getCharacter);

                //getCharacterVector
                final char[] getCharacterVector = results.getCharacterVector("getCharacterVector");
                Assertions.assertThat(putCharacterVector).isEqualTo(getCharacterVector);

                //getCharacterVectorAsList
                final List<Character> getCharacterVectorAsList = results
                        .getCharacterVectorAsList("getCharacterVectorAsList");
                Assertions.assertThat(putCharacterVectorAsList).isEqualTo(getCharacterVectorAsList);

                //getCharacterMatrix
                final char[][] getCharacterMatrix = results.getCharacterMatrix("getCharacterMatrix");
                Assertions.assertThat(putCharacterMatrix).isEqualTo(getCharacterMatrix);

                //getCharacterMatrixAsList
                final List<List<Character>> getCharacterMatrixAsList = results
                        .getCharacterMatrixAsList("getCharacterMatrixAsList");
                Assertions.assertThat(putCharacterMatrixAsList).isEqualTo(getCharacterMatrixAsList);
                return null;
            }
        }.run(runner);
    }

}
