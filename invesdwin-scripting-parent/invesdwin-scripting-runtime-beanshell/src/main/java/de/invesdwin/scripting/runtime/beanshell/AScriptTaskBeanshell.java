package de.invesdwin.scripting.runtime.beanshell;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskBeanshell<V> extends AScriptTask<V, IScriptTaskRunnerBeanshell> {

    @Override
    public V run(final IScriptTaskRunnerBeanshell runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ScriptTaskRunnerBeanshell.INSTANCE);
    }

}
