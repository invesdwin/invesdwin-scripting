package de.invesdwin.scripting.julia.runtime.julia4j;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.julia.runtime.contract.InputsAndResultsTests;
import de.invesdwin.scripting.julia.runtime.contract.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.julia.runtime.contract.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

// LD_PRELOAD=/usr/lib/jvm/default-java/lib/libjsig.so
@NotThreadSafe
public class Julia4jScriptTaskRunnerJuliaTest extends ATest {

    @Inject
    private Julia4jScriptTaskRunnerJulia runner;

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
