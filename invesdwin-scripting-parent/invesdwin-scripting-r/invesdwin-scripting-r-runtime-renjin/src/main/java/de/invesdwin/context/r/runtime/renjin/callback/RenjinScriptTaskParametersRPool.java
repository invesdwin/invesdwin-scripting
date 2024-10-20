package de.invesdwin.context.r.runtime.renjin.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class RenjinScriptTaskParametersRPool extends AAgronaObjectPool<RenjinScriptTaskParametersR> {

    public static final RenjinScriptTaskParametersRPool INSTANCE = new RenjinScriptTaskParametersRPool();

    private RenjinScriptTaskParametersRPool() {}

    @Override
    protected RenjinScriptTaskParametersR newObject() {
        return new RenjinScriptTaskParametersR();
    }

    @Override
    protected boolean passivateObject(final RenjinScriptTaskParametersR element) {
        element.close();
        return true;
    }

}
