package de.invesdwin.context.r.runtime.renjin;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.r.runtime.contract.InputsAndResultsTests;
import de.invesdwin.context.r.runtime.contract.callback.ParametersAndReturnsTests;
import de.invesdwin.context.r.runtime.contract.callback.SimpleCallbackTest;
import de.invesdwin.context.test.ATest;
import jakarta.inject.Inject;

@NotThreadSafe
public class RenjinScriptTaskRunnerRTest extends ATest {

    @Inject
    private RenjinScriptTaskRunnerR runner;

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
