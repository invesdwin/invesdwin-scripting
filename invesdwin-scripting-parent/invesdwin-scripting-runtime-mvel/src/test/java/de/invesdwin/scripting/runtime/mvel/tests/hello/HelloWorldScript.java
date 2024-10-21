package de.invesdwin.scripting.runtime.mvel.tests.hello;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.runtime.mvel.AScriptTaskMvel;
import de.invesdwin.scripting.runtime.mvel.IScriptTaskRunnerMvel;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class HelloWorldScript {

    private final IScriptTaskRunnerMvel runner;

    public HelloWorldScript(final IScriptTaskRunnerMvel runner) {
        this.runner = runner;
    }

    public void testHelloWorld() {
        final AScriptTaskMvel<String> script = new AScriptTaskMvel<String>() {

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {
                inputs.putString("hello", "World");
            }

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                //execute this script inline:
                //                engine.eval("world = \"Hello \" + hello + \"!\"");
                //or run it from a file:
                engine.eval(new ClassPathResource(HelloWorldScript.class.getSimpleName() + ".mvel", getClass()));
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
