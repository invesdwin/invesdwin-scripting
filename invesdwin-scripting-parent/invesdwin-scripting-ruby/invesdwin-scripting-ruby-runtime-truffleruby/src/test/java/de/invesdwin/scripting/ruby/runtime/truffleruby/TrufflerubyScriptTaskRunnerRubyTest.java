package de.invesdwin.scripting.ruby.runtime.truffleruby;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.ruby.runtime.tests.InputsAndResultsTests;
import de.invesdwin.scripting.ruby.runtime.tests.callback.ParametersAndReturnsTests;
import jakarta.inject.Inject;

@NotThreadSafe
public class TrufflerubyScriptTaskRunnerRubyTest extends ATest {

    @Inject
    private TrufflerubyScriptTaskRunnerRuby runner;

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
