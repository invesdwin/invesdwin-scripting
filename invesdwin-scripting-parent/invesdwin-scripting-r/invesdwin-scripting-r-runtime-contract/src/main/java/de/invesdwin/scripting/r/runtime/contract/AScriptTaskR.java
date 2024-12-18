package de.invesdwin.scripting.r.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskR<V> extends AScriptTask<V, IScriptTaskRunnerR> {

    @Override
    public V run(final IScriptTaskRunnerR runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerR.INSTANCE);
    }

}
