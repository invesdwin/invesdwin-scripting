package de.invesdwin.scripting.runtime.kotlin;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskKotlin<V> extends AScriptTask<V, IScriptTaskRunnerKotlin> {

    @Override
    public V run(final IScriptTaskRunnerKotlin runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerKotlin.INSTANCE);
    }

}
