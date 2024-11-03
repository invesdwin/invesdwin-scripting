package de.invesdwin.scripting.rust.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskParametersRustFromJsonPool
        extends AAgronaObjectPool<ScriptTaskParametersRustFromJson> {

    public static final ScriptTaskParametersRustFromJsonPool INSTANCE = new ScriptTaskParametersRustFromJsonPool();

    private ScriptTaskParametersRustFromJsonPool() {}

    @Override
    protected ScriptTaskParametersRustFromJson newObject() {
        return new ScriptTaskParametersRustFromJson();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskParametersRustFromJson element) {
        element.close();
        return true;
    }

}
