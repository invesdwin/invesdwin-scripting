package de.invesdwin.scripting.haskell.runtime.contract.callback;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class ScriptTaskReturnsHaskellToExpressionPool
        extends AAgronaObjectPool<ScriptTaskReturnsHaskellToExpression> {

    public static final ScriptTaskReturnsHaskellToExpressionPool INSTANCE = new ScriptTaskReturnsHaskellToExpressionPool();

    private ScriptTaskReturnsHaskellToExpressionPool() {}

    @Override
    protected ScriptTaskReturnsHaskellToExpression newObject() {
        return new ScriptTaskReturnsHaskellToExpression();
    }

    @Override
    protected boolean passivateObject(final ScriptTaskReturnsHaskellToExpression element) {
        element.close();
        return true;
    }

}
