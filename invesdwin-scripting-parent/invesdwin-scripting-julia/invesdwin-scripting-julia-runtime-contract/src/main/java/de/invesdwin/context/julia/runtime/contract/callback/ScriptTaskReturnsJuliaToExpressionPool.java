package de.invesdwin.context.julia.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskReturnsJuliaToExpressionPool
        extends AAgronaObjectPool<ScriptTaskReturnsJuliaToExpression> {

    public static final ScriptTaskReturnsJuliaToExpressionPool INSTANCE = new ScriptTaskReturnsJuliaToExpressionPool();

    private ScriptTaskReturnsJuliaToExpressionPool() {}

    @Override
    protected ScriptTaskReturnsJuliaToExpression newObject() {
        return new ScriptTaskReturnsJuliaToExpression();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskReturnsJuliaToExpression element) {
        element.close();
        return true;
    }

}
