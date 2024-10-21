package de.invesdwin.scripting.haskell.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskHaskell<V> extends AScriptTask<V, IScriptTaskRunnerHaskell> {

    @Override
    public V run(final IScriptTaskRunnerHaskell runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerHaskell.INSTANCE);
    }

}
