package de.invesdwin.context.r.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;
import jakarta.inject.Named;

import de.invesdwin.context.r.runtime.contract.ProvidedScriptTaskRunnerR;
import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerRStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerR.setProvidedInstance(null);
    }

}
