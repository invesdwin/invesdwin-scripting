package de.invesdwin.scripting.runtime.jshell;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.runtime.jshell.tests.InputsAndResultsTests;
import de.invesdwin.scripting.runtime.jshell.tests.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.runtime.jshell.tests.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class ScriptTaskRunnerJshellTest extends ATest {

    @Inject
    private ScriptTaskRunnerJshell runner;

    @Test
    public void test() {
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
