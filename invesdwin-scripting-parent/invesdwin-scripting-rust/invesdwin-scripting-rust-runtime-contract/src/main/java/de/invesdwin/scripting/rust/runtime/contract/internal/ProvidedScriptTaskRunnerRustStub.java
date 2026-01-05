package de.invesdwin.scripting.rust.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.ITestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.rust.runtime.contract.ProvidedScriptTaskRunnerRust;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerRustStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final ITestContext ctx) {
        if (!ctx.isFinishedGlobal()) {
            return;
        }
        ProvidedScriptTaskRunnerRust.setProvidedInstance(null);
    }

}
