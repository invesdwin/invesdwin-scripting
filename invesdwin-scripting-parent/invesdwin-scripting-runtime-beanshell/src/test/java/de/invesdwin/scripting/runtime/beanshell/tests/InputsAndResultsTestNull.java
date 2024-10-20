package de.invesdwin.scripting.runtime.beanshell.tests;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.runtime.beanshell.AScriptTaskBeanshell;
import de.invesdwin.scripting.runtime.beanshell.IScriptTaskRunnerBeanshell;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class InputsAndResultsTestNull {

    private final IScriptTaskRunnerBeanshell runner;

    public InputsAndResultsTestNull(final IScriptTaskRunnerBeanshell runner) {
        this.runner = runner;
    }

    public void testNull() {
        new AScriptTaskBeanshell<Void>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
            }

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
