package de.invesdwin.scripting.r.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.TestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.r.runtime.contract.ProvidedScriptTaskRunnerR;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerRStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final TestContext ctx) {
        if (!ctx.isFinishedGlobal()) {
            return;
        }
        ProvidedScriptTaskRunnerR.setProvidedInstance(null);
    }

}
