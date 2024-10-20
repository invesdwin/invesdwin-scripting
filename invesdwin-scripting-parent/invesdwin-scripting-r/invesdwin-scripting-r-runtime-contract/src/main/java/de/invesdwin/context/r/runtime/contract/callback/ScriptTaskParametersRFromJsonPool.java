package de.invesdwin.context.r.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskParametersRFromJsonPool
        extends AAgronaObjectPool<ScriptTaskParametersRFromJson> {

    public static final ScriptTaskParametersRFromJsonPool INSTANCE = new ScriptTaskParametersRFromJsonPool();

    private ScriptTaskParametersRFromJsonPool() {}

    @Override
    protected ScriptTaskParametersRFromJson newObject() {
        return new ScriptTaskParametersRFromJson();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskParametersRFromJson element) {
        element.close();
        return true;
    }

}
