package de.invesdwin.scripting.python.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.ITestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.python.runtime.contract.ProvidedScriptTaskRunnerPython;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerPythonStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final ITestContext ctx) {
        if (!ctx.isFinishedGlobal()) {
            return;
        }
        ProvidedScriptTaskRunnerPython.setProvidedInstance(null);
    }

}
