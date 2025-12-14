package de.invesdwin.scripting.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.haskell.runtime.contract.InputsAndResultsTests;
import de.invesdwin.scripting.haskell.runtime.contract.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class GhciScriptTaskRunnerHaskellTest extends ATest {

    @Inject
    private GhciScriptTaskRunnerHaskell runner;

    @Test
    public void test() {
        new InputsAndResultsTests(runner).test();
    }

    @Test
    public void testParallel() {
        new InputsAndResultsTests(runner).testParallel();
    }

    //    @Test
    //    public void testCallback() {
    //        new ParametersAndReturnsTests(runner).test();
    //    }

    //    @Test
    //    public void testCallbackParallel() {
    //        new ParametersAndReturnsTests(runner).testParallel();
    //    }

    @Disabled("TODO: still needs more development")
    @Test
    public void testSimpleCallback() {
        new SimpleCallbackTest(runner).testSimpleCallback();
    }

}
