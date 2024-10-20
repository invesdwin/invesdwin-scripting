package de.invesdwin.context.haskell.runtime.contract.callback;

import java.io.Closeable;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ScriptTaskReturnsHaskellToExpression extends AScriptTaskReturnsHaskellToExpression implements Closeable {

    private String returnExpression;

    public String getReturnExpression() {
        return returnExpression;
    }

    @Override
    public void returnExpression(final String expression) {
        assert returnExpression == null;
        this.returnExpression = expression;
    }

    @Override
    public void close() {
        returnExpression = null;
    }

    @Override
    public String toString() {
        return returnExpression;
    }

}
