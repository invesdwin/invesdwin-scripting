package de.invesdwin.scripting.runtime.jshell;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskJshell<V> extends AScriptTask<V, IScriptTaskRunnerJshell> {

    @Override
    public V run(final IScriptTaskRunnerJshell runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerJshell.INSTANCE);
    }

}
