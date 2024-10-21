package de.invesdwin.scripting.runtime.beanshell;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.runtime.beanshell.tests.InputsAndResultsTests;
import de.invesdwin.scripting.runtime.beanshell.tests.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.runtime.beanshell.tests.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class ScriptTaskRunnerBeanshellTest extends ATest {

    @Inject
    private ScriptTaskRunnerBeanshell runner;

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
