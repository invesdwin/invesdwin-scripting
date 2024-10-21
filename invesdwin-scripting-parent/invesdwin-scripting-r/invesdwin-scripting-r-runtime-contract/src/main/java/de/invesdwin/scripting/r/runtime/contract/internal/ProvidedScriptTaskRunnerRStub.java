package de.invesdwin.scripting.r.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.r.runtime.contract.ProvidedScriptTaskRunnerR;
import jakarta.inject.Named;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerRStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerR.setProvidedInstance(null);
    }

}
