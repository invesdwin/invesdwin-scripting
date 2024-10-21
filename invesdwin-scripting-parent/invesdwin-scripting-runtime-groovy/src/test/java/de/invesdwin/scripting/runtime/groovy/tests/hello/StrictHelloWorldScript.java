package de.invesdwin.scripting.runtime.groovy.tests.hello;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.runtime.groovy.AScriptTaskGroovy;
import de.invesdwin.scripting.runtime.groovy.IScriptTaskRunnerGroovy;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class StrictHelloWorldScript {

    private final IScriptTaskRunnerGroovy runner;

    public StrictHelloWorldScript(final IScriptTaskRunnerGroovy runner) {
        this.runner = runner;
    }

    public void testHelloWorld() {
        final AScriptTaskGroovy<String> script = new AScriptTaskGroovy<String>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
                inputs.putString("hello", "World");
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(
                        new ClassPathResource(StrictHelloWorldScript.class.getSimpleName() + ".groovy", getClass()));
            }

            @Override
            public String extractResults(final IScriptTaskResults results) {
                return results.getString("world");
            }
        };
        final String result = script.run(runner);
        Assertions.assertThat(result).isEqualTo("Hello World!");
    }

}
