package de.invesdwin.scripting.haskell.runtime.contract.internal;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.TestContext;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.haskell.runtime.contract.ProvidedScriptTaskRunnerHaskell;
import jakarta.inject.Named;

@Named
@Immutable
public class ProvidedScriptTaskRunnerHaskellStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test, final TestContext ctx) {
        if (!ctx.isFinishedGlobal()) {
            return;
        }
        ProvidedScriptTaskRunnerHaskell.setProvidedInstance(null);
    }

}
