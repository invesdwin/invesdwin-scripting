package de.invesdwin.context.r.runtime.renjin.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class RenjinScriptTaskReturnsRPool extends AAgronaObjectPool<RenjinScriptTaskReturnsR> {

    public static final RenjinScriptTaskReturnsRPool INSTANCE = new RenjinScriptTaskReturnsRPool();

    private RenjinScriptTaskReturnsRPool() {}

    @Override
    protected RenjinScriptTaskReturnsR newObject() {
        return new RenjinScriptTaskReturnsR();
    }

    @Override
    protected boolean passivateObject(final RenjinScriptTaskReturnsR element) {
        element.close();
        return true;
    }

}
