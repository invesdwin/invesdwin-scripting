package de.invesdwin.scripting.runtime.lua;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.runtime.lua.tests.InputsAndResultsTests;
import de.invesdwin.scripting.runtime.lua.tests.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.runtime.lua.tests.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class ScriptTaskRunnerLuaTest extends ATest {

    @Inject
    private ScriptTaskRunnerLua runner;

    @Test
    public void test() {
        //TODO: run tests in multiple engines by setting the system property and including the test deps, similar to the groovy strict tests
        new InputsAndResultsTests(runner).test();
    }

    @Test
    public void testParallel() {
        new InputsAndResultsTests(runner).testParallel();
    }

    @Test
    public void testCallback() {
        new ParametersAndReturnsTests(runner).test();
    }

    @Test
    public void testCallbackParallel() {
        new ParametersAndReturnsTests(runner).testParallel();
    }

    @Test
    public void testSimpleCallback() {
        new SimpleCallbackTest(runner).testSimpleCallback();
    }

}
