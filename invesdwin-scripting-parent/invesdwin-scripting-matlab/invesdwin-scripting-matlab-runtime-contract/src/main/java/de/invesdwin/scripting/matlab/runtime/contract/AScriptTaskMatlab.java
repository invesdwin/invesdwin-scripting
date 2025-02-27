package de.invesdwin.scripting.matlab.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskMatlab<V> extends AScriptTask<V, IScriptTaskRunnerMatlab> {

    @Override
    public V run(final IScriptTaskRunnerMatlab runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerMatlab.INSTANCE);
    }

}
