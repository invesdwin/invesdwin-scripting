package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.scripting.matlab.runtime.javasci.test.InputsAndResultsTests;
import de.invesdwin.scripting.matlab.runtime.javasci.test.callback.ParametersAndReturnsTests;
import de.invesdwin.scripting.matlab.runtime.javasci.test.callback.SimpleCallbackTest;
import jakarta.inject.Inject;

@NotThreadSafe
public class JascibScriptTaskRunnerMatlabTest extends ATest {

	@Inject
	private JascibScriptTaskRunnerMatlab runner;

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
