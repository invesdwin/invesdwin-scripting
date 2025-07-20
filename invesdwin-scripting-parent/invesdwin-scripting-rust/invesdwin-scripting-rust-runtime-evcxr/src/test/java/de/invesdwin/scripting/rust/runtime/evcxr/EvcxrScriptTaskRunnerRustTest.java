package de.invesdwin.scripting.rust.runtime.evcxr;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.rust.runtime.contract.InputsAndResultsTests;
import de.invesdwin.scripting.rust.runtime.contract.callback.ParametersAndReturnsTests;
import jakarta.inject.Inject;

@NotThreadSafe
public class EvcxrScriptTaskRunnerRustTest extends ATest {

    @Inject
    private EvcxrScriptTaskRunnerRust runner;

    @Test
    public void test() {
        new InputsAndResultsTests(runner).test();
    }

    @Disabled("very slow, thus disabled per default")
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
