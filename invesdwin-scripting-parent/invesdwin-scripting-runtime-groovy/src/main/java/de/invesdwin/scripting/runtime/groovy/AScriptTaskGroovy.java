package de.invesdwin.scripting.runtime.groovy;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskGroovy<V> extends AScriptTask<V, IScriptTaskRunnerGroovy> {

    @Override
    public V run(final IScriptTaskRunnerGroovy runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerGroovy.INSTANCE);
    }

}
