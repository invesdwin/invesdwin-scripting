package de.invesdwin.scripting.runtime.groovy.tests.callback.hello;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.runtime.groovy.AScriptTaskGroovy;
import de.invesdwin.scripting.runtime.groovy.IScriptTaskRunnerGroovy;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class CallbackStrictHelloWorldScript {

    private final IScriptTaskRunnerGroovy runner;

    public CallbackStrictHelloWorldScript(final IScriptTaskRunnerGroovy runner) {
        this.runner = runner;
    }

    public void testHelloWorld() {
        final CallbackStrictHelloWorldScriptCallback callback = new CallbackStrictHelloWorldScriptCallback();
        final AScriptTaskGroovy<String> script = new AScriptTaskGroovy<String>() {

            @Override
            public IScriptTaskCallback getCallback() {
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(CallbackStrictHelloWorldScript.class.getSimpleName() + ".groovy",
                        getClass()));
            }

            @Override
            public String extractResults(final IScriptTaskResults results) {
                return callback.world;
            }
        };
        final String result = script.run(runner);
        Assertions.assertThat(result).isEqualTo("Hello World!");
    }

    public static class CallbackStrictHelloWorldScriptCallback {

        private String world = null;

        public String hello() {
            return "World";
        }

        public void world(final String world) {
            this.world = world;
        }

    }

}
