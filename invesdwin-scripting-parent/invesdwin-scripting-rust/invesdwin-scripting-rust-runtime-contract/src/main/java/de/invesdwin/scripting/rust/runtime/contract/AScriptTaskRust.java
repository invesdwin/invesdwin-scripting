package de.invesdwin.scripting.rust.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskRust<V> extends AScriptTask<V, IScriptTaskRunnerRust> {

    @Override
    public V run(final IScriptTaskRunnerRust runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerRust.INSTANCE);
    }

}
