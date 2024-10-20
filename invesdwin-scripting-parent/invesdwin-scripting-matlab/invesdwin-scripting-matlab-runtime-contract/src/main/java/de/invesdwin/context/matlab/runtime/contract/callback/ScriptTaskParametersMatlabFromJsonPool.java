package de.invesdwin.context.matlab.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskParametersMatlabFromJsonPool
        extends AAgronaObjectPool<ScriptTaskParametersMatlabFromJson> {

    public static final ScriptTaskParametersMatlabFromJsonPool INSTANCE = new ScriptTaskParametersMatlabFromJsonPool();

    private ScriptTaskParametersMatlabFromJsonPool() {}

    @Override
    protected ScriptTaskParametersMatlabFromJson newObject() {
        return new ScriptTaskParametersMatlabFromJson();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskParametersMatlabFromJson element) {
        element.close();
        return true;
    }

}
