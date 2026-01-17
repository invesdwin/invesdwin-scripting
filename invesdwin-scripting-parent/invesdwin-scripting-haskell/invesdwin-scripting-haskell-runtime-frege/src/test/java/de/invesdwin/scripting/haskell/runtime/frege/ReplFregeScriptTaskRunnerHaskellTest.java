package de.invesdwin.scripting.haskell.runtime.frege;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.haskell.runtime.contract.InputsAndResultsTests;
import de.invesdwin.scripting.haskell.runtime.frege.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.haskell.runtime.frege.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class ReplFregeScriptTaskRunnerHaskellTest extends ATest {

    @Inject
    private FregeScriptTaskRunnerHaskell runner;

    @Override
    public void setUp() throws Exception {
        super.setUp();
        FregeProperties.setForkedReplProcess(true);
    }

    @Override
    public void tearDown() throws Exception {
        super.tearDown();
        FregeProperties.setForkedReplProcess(null);
    }

    @Test
    public void test() {
        new InputsAndResultsTests(runner).test();
    }

    @Disabled("frege is too slow")
    @Test
    public void testParallel() {
        new InputsAndResultsTests(runner).testParallel();
    }

    @Test
    public void testCallback() {
        new ParametersAndReturnsTests(runner).test();
    }

    @Disabled("frege is too slow")
    @Test
    public void testCallbackParallel() {
        new ParametersAndReturnsTests(runner).testParallel();
    }

    @Test
    public void testSimpleCallback() {
        new SimpleCallbackTest(runner).testSimpleCallback();
    }

}
