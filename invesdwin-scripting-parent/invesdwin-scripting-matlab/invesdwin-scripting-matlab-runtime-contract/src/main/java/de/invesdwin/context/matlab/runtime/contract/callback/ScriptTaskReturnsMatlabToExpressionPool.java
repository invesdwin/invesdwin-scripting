package de.invesdwin.context.matlab.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskReturnsMatlabToExpressionPool
        extends AAgronaObjectPool<ScriptTaskReturnsMatlabToExpression> {

    public static final ScriptTaskReturnsMatlabToExpressionPool INSTANCE = new ScriptTaskReturnsMatlabToExpressionPool();

    private ScriptTaskReturnsMatlabToExpressionPool() {}

    @Override
    protected ScriptTaskReturnsMatlabToExpression newObject() {
        return new ScriptTaskReturnsMatlabToExpression();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskReturnsMatlabToExpression element) {
        element.close();
        return true;
    }

}
