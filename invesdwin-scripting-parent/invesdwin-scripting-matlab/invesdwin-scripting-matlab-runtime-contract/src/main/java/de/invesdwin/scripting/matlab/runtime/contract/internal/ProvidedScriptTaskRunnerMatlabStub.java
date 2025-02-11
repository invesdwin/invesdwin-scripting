package de.invesdwin.scripting.matlab.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.matlab.runtime.contract.ProvidedScriptTaskRunnerMatlab;
import jakarta.inject.Named;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerMatlabStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerMatlab.setProvidedInstance(null);
    }

}
