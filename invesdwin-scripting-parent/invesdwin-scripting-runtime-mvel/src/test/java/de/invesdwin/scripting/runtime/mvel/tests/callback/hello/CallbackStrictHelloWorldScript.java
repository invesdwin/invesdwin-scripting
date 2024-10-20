package de.invesdwin.scripting.runtime.mvel.tests.callback.hello;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.runtime.mvel.AScriptTaskMvel;
import de.invesdwin.scripting.runtime.mvel.IScriptTaskRunnerMvel;
import de.invesdwin.util.assertions.Assertions;

@NotThreadSafe
public class CallbackStrictHelloWorldScript {

    private final IScriptTaskRunnerMvel runner;

    public CallbackStrictHelloWorldScript(final IScriptTaskRunnerMvel runner) {
        this.runner = runner;
    }

    public void testHelloWorld() {
        final CallbackStrictHelloWorldScriptCallback callback = new CallbackStrictHelloWorldScriptCallback();
        final AScriptTaskMvel<String> script = new AScriptTaskMvel<String>() {

            @Override
            public IScriptTaskCallback getCallback() {
                return new ReflectiveScriptTaskCallback(callback);
            }

            @Override
            public void populateInputs(final IScriptTaskInputs inputs) {}

            @Override
            public void executeScript(final IScriptTaskEngine engine) {
                engine.eval(new ClassPathResource(CallbackStrictHelloWorldScript.class.getSimpleName() + ".mvel",
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
