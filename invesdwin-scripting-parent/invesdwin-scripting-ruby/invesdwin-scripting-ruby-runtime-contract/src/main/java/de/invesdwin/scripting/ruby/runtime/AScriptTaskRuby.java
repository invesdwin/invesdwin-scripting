package de.invesdwin.scripting.ruby.runtime;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.AScriptTask;

@NotThreadSafe
public abstract class AScriptTaskRuby<V> extends AScriptTask<V, IScriptTaskRunnerRuby> {

    @Override
    public V run(final IScriptTaskRunnerRuby runner) {
        return runner.run(this);
    }

    @Override
    public V run() {
        return run(ProvidedScriptTaskRunnerRuby.INSTANCE);
    }

}
