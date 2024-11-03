package de.invesdwin.scripting.rust.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskReturnsRustToExpressionPool
        extends AAgronaObjectPool<ScriptTaskReturnsRustToExpression> {

    public static final ScriptTaskReturnsRustToExpressionPool INSTANCE = new ScriptTaskReturnsRustToExpressionPool();

    private ScriptTaskReturnsRustToExpressionPool() {}

    @Override
    protected ScriptTaskReturnsRustToExpression newObject() {
        return new ScriptTaskReturnsRustToExpression();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskReturnsRustToExpression element) {
        element.close();
        return true;
    }

}
