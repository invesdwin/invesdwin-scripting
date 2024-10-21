package de.invesdwin.scripting.julia.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.julia.runtime.contract.ProvidedScriptTaskRunnerJulia;
import jakarta.inject.Named;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerJuliaStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerJulia.setProvidedInstance(null);
    }

}
