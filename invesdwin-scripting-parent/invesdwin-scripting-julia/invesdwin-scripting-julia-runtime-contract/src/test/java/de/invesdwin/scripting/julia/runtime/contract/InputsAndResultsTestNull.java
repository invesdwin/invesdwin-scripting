package de.invesdwin.scripting.julia.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class InputsAndResultsTestNull {

    private final IScriptTaskRunnerJulia runner;

    public InputsAndResultsTestNull(final IScriptTaskRunnerJulia runner) {
        this.runner = runner;
    }

    public void testNull() {
        new AScriptTaskJulia<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                final IScriptTaskResultsJulia cResults = (IScriptTaskResultsJulia) engine.getResults();
                //the variable might have been defined by a previous run, thus allow null too
                Assertions.checkTrue(cResults.isNotDefinedOrNull("testVariable"));
                Assertions.checkFalse(cResults.isDefinedNotNull("testVariable"));
                engine.getInputs().putNull("testVariable");
                Assertions.checkFalse(cResults.isNotDefined("testVariable"));
                Assertions.checkTrue(cResults.isDefined("testVariable"));
                Assertions.checkTrue(cResults.isNull("testVariable"));
                Assertions.checkFalse(cResults.isNotNull("testVariable"));
                engine.getInputs().putString("testVariable", "value");
                Assertions.checkFalse(cResults.isNotDefined("testVariable"));
                Assertions.checkTrue(cResults.isDefined("testVariable"));
                Assertions.checkFalse(cResults.isNull("testVariable"));
                Assertions.checkTrue(cResults.isNotNull("testVariable"));
                //there is no remove in julia, thus we have to check for null as well
                engine.getInputs().remove("testVariable");
                Assertions.checkTrue(cResults.isNotDefinedOrNull("testVariable"));
                Assertions.checkFalse(cResults.isDefinedNotNull("testVariable"));

            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

}
