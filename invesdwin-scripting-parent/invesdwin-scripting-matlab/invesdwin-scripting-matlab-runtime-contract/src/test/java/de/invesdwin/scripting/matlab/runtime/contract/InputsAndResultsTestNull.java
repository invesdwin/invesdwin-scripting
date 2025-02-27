package de.invesdwin.scripting.matlab.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class InputsAndResultsTestNull {

    private final IScriptTaskRunnerMatlab runner;

    public InputsAndResultsTestNull(final IScriptTaskRunnerMatlab runner) {
        this.runner = runner;
    }

    public void testNull() {
        new AScriptTaskMatlab<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                Assertions.checkTrue(engine.getResults().isNotDefined("testVariable"));
                Assertions.checkFalse(engine.getResults().isDefined("testVariable"));
                engine.getInputs().putNull("testVariable");
                Assertions.checkFalse(engine.getResults().isNotDefined("testVariable"));
                Assertions.checkTrue(engine.getResults().isDefined("testVariable"));
                Assertions.checkTrue(engine.getResults().isNull("testVariable"));
                Assertions.checkFalse(engine.getResults().isNotNull("testVariable"));
                engine.getInputs().putString("testVariable", "value");
                Assertions.checkFalse(engine.getResults().isNotDefined("testVariable"));
                Assertions.checkTrue(engine.getResults().isDefined("testVariable"));
                Assertions.checkFalse(engine.getResults().isNull("testVariable"));
                Assertions.checkTrue(engine.getResults().isNotNull("testVariable"));
                engine.getInputs().remove("testVariable");
                Assertions.checkTrue(engine.getResults().isNotDefined("testVariable"));
                Assertions.checkFalse(engine.getResults().isDefined("testVariable"));

            }

            @Override
            public Void extractResults(final IScriptTaskResults results) {
                return null;
            }
        }.run(runner);
    }

}
