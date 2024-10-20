package de.invesdwin.context.matlab.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;
import jakarta.inject.Named;

import de.invesdwin.context.matlab.runtime.contract.ProvidedScriptTaskRunnerMatlab;
import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerMatlabStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerMatlab.setProvidedInstance(null);
    }

}
