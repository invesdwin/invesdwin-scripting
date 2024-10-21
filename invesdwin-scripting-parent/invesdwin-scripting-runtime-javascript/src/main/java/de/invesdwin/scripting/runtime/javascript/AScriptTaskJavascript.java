package de.invesdwin.scripting.runtime.javascript;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskJavascript<V> extends AScriptTask<V, IScriptTaskRunnerJavascript> {

    @Override
    public V run(final IScriptTaskRunnerJavascript runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerJavascript.INSTANCE);
    }

}
