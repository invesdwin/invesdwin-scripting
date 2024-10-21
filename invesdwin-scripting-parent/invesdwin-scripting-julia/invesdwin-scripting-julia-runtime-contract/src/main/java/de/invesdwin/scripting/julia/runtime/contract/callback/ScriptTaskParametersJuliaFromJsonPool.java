package de.invesdwin.scripting.julia.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskParametersJuliaFromJsonPool
        extends AAgronaObjectPool<ScriptTaskParametersJuliaFromJson> {

    public static final ScriptTaskParametersJuliaFromJsonPool INSTANCE = new ScriptTaskParametersJuliaFromJsonPool();

    private ScriptTaskParametersJuliaFromJsonPool() {}

    @Override
    protected ScriptTaskParametersJuliaFromJson newObject() {
        return new ScriptTaskParametersJuliaFromJson();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskParametersJuliaFromJson element) {
        element.close();
        return true;
    }

}
