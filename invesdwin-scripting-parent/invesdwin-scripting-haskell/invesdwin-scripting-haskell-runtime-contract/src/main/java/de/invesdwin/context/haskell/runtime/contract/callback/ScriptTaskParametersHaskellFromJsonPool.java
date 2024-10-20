package de.invesdwin.context.haskell.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskParametersHaskellFromJsonPool
        extends AAgronaObjectPool<ScriptTaskParametersHaskellFromJson> {

    public static final ScriptTaskParametersHaskellFromJsonPool INSTANCE = new ScriptTaskParametersHaskellFromJsonPool();

    private ScriptTaskParametersHaskellFromJsonPool() {}

    @Override
    protected ScriptTaskParametersHaskellFromJson newObject() {
        return new ScriptTaskParametersHaskellFromJson();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskParametersHaskellFromJson element) {
        element.close();
        return true;
    }

}
