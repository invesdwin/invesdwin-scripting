package de.invesdwin.scripting.python.runtime.contract.internal;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.test.ATest;
import de.invesdwin.context.test.stub.StubSupport;
import de.invesdwin.scripting.python.runtime.contract.ProvidedScriptTaskRunnerPython;
import jakarta.inject.Named;

@Named
@NotThreadSafe
public class ProvidedScriptTaskRunnerPythonStub extends StubSupport {

    @Override
    public void tearDownOnce(final ATest test) throws Exception {
        ProvidedScriptTaskRunnerPython.setProvidedInstance(null);
    }

}
