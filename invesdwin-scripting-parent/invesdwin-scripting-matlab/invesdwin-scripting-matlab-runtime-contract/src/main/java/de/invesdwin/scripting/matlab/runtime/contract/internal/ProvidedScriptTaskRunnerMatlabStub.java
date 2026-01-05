package de.invesdwin.scripting.matlab.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.ITestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.matlab.runtime.contract.ProvidedScriptTaskRunnerMatlab;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerMatlabStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final ITestContext ctx) {
        if (!ctx.isFinishedGlobal()) {
            return;
        }
        ProvidedScriptTaskRunnerMatlab.setProvidedInstance(null);
    }

}
