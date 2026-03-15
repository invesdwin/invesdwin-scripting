package de.invesdwin.scripting.runtime.lua;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskLua<V> extends AScriptTask<V, IScriptTaskRunnerLua> {

    @Override
    public V run(final IScriptTaskRunnerLua runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerLua.INSTANCE);
    }

}
