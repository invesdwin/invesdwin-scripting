package de.invesdwin.context.julia.runtime.contract;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.integration.script.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskJulia<V> extends AScriptTask<V, IScriptTaskRunnerJulia> {

    @Override
    public V run(final IScriptTaskRunnerJulia runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerJulia.INSTANCE);
    }

}
