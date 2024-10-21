package de.invesdwin.scripting.runtime.clojure;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskClojure<V> extends AScriptTask<V, IScriptTaskRunnerClojure> {

    @Override
    public V run(final IScriptTaskRunnerClojure runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerClojure.INSTANCE);
    }

}
