package de.invesdwin.context.julia.runtime.jajub;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.julia.runtime.contract.InputsAndResultsTests;
import de.invesdwin.context.julia.runtime.contract.callback.ParametersAndReturnsTests;
import de.invesdwin.context.julia.runtime.contract.callback.SimpleCallbackTest;
import de.invesdwin.context.test.ATest;
import jakarta.inject.Inject;

@NotThreadSafe
public class JajubScriptTaskRunnerJuliaTest extends ATest {

    @Inject
    private JajubScriptTaskRunnerJulia runner;

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
