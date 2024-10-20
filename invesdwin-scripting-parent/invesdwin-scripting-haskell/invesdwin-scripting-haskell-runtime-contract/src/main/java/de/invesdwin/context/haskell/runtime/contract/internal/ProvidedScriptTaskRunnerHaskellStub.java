package de.invesdwin.context.haskell.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;
import jakarta.inject.Named;
import de.invesdwin.context.haskell.runtime.contract.ProvidedScriptTaskRunnerHaskell;
import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerHaskellStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerHaskell.setProvidedInstance(null);
    }

}
