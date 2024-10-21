package de.invesdwin.scripting.python.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskPython<V> extends AScriptTask<V, IScriptTaskRunnerPython> {

    @Override
    public V run(final IScriptTaskRunnerPython runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerPython.INSTANCE);
    }

}
