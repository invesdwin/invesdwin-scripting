package de.invesdwin.scripting.rust.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.rust.runtime.contract.ProvidedScriptTaskRunnerRust;
import jakarta.inject.Named;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerRustStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerRust.setProvidedInstance(null);
    }

}
