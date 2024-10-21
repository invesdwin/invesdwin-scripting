package de.invesdwin.scripting.runtime.scala;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.runtime.scala.tests.InputsAndResultsTests;
import de.invesdwin.scripting.runtime.scala.tests.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.runtime.scala.tests.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class ScriptTaskRunnerScalaTest extends ATest {

    @Inject
    private ScriptTaskRunnerScala runner;

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
