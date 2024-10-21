package de.invesdwin.scripting.haskell.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class InputsAndResultsTestNull {

    private final IScriptTaskRunnerHaskell runner;

    public InputsAndResultsTestNull(final IScriptTaskRunnerHaskell runner) {
        this.runner = runner;
    }

    public void testNull() {
        new AScriptTaskHaskell<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                final IScriptTaskResultsHaskell cResults = (IScriptTaskResultsHaskell) engine.getResults();
                //Frege does not support checking for undefined variables, also variables can only be set once (only constants are supported actually)
                //                Assertions.checkTrue(cResults.isNotDefinedOrNull("testVariable"));
                //                Assertions.checkFalse(cResults.isDefinedNotNull("testVariable"));
                //we treat null (empty string in frege because Nothing/null does not work) as undefined
                engine.getInputs().putNull("testVariableNull");
                Assertions.checkTrue(cResults.isNotDefined("testVariableNull"));
                Assertions.checkFalse(cResults.isDefined("testVariableNull"));
                Assertions.checkTrue(cResults.isNull("testVariableNull"));
                Assertions.checkFalse(cResults.isNotNull("testVariableNull"));
                engine.getInputs().putString("testVariableValue", "value");
                Assertions.checkFalse(cResults.isNotDefined("testVariableValue"));
                Assertions.checkTrue(cResults.isDefined("testVariableValue"));
                Assertions.checkFalse(cResults.isNull("testVariableValue"));
                Assertions.checkTrue(cResults.isNotNull("testVariableValue"));
                //there is no remove in Haskell, thus we have to check for null as well
                engine.getInputs().remove("testVariableRemove");
                Assertions.checkTrue(cResults.isNotDefinedOrNull("testVariableRemove"));
                Assertions.checkFalse(cResults.isDefinedNotNull("testVariableRemove"));

            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

}
