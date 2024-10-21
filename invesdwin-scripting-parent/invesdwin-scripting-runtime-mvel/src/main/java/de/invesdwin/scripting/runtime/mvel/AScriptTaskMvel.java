package de.invesdwin.scripting.runtime.mvel;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskMvel<V> extends AScriptTask<V, IScriptTaskRunnerMvel> {

    @Override
    public V run(final IScriptTaskRunnerMvel runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerMvel.INSTANCE);
    }

}
