package de.invesdwin.context.r.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskReturnsRToExpressionPool
        extends AAgronaObjectPool<ScriptTaskReturnsRToExpression> {

    public static final ScriptTaskReturnsRToExpressionPool INSTANCE = new ScriptTaskReturnsRToExpressionPool();

    private ScriptTaskReturnsRToExpressionPool() {}

    @Override
    protected ScriptTaskReturnsRToExpression newObject() {
        return new ScriptTaskReturnsRToExpression();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskReturnsRToExpression element) {
        element.close();
        return true;
    }

}
