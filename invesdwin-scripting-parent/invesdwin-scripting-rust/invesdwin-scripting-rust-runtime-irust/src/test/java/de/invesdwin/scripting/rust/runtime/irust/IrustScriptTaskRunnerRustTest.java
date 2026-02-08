package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.rust.runtime.contract.InputsAndResultsTests;
import de.invesdwin.scripting.rust.runtime.contract.callback.ParametersAndReturnsTests;
import jakarta.inject.Inject;

@NotThreadSafe
public class IrustScriptTaskRunnerRustTest extends ATest {

    @Inject
    private IrustScriptTaskRunnerRust runner;

    @Test
    public void test() {
        new InputsAndResultsTests(runner).test();
    }

    @Disabled("very slow, thus disabled per default")
    @Test
    public void testParallel() {
        new InputsAndResultsTests(runner).testParallel();
    }

    @Disabled("not yet implemented")
    @Test
    public void testCallback() {
        new ParametersAndReturnsTests(runner).test();
    }

    @Disabled("not yet implemented")
    @Test
    public void testCallbackParallel() {
        new ParametersAndReturnsTests(runner).testParallel();
    }

    //@Disabled("not yet implemented")
    @Test
    public void testSimpleCallback() {
        new SimpleCallbackTest(runner).testSimpleCallback();
    }

}
