package de.invesdwin.scripting.rust.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.TestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.rust.runtime.contract.ProvidedScriptTaskRunnerRust;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerRustStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final TestContext ctx) {
        if (!ctx.isFinished()) {
            return;
        }
        ProvidedScriptTaskRunnerRust.setProvidedInstance(null);
    }

}
