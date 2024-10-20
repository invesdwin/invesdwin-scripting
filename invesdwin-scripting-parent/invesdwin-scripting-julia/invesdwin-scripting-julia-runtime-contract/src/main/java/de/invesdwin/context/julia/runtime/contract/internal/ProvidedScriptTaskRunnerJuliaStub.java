package de.invesdwin.context.julia.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;
import jakarta.inject.Named;

import de.invesdwin.context.julia.runtime.contract.ProvidedScriptTaskRunnerJulia;
import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerJuliaStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerJulia.setProvidedInstance(null);
    }

}
