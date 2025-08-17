package de.invesdwin.scripting.matlab.runtime.jascib.callback.file;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class JascibScriptTaskReturnsMatlabToExpressionPool
        extends AAgronaObjectPool<JascibScriptTaskReturnsMatlabToExpression> {

    public static final JascibScriptTaskReturnsMatlabToExpressionPool INSTANCE = new JascibScriptTaskReturnsMatlabToExpressionPool();

    private JascibScriptTaskReturnsMatlabToExpressionPool() {}

    @Override
    protected JascibScriptTaskReturnsMatlabToExpression newObject() {
        return new JascibScriptTaskReturnsMatlabToExpression();
    }

    @Override
    protected boolean passivateObject(final JascibScriptTaskReturnsMatlabToExpression element) {
        element.close();
        return true;
    }

}
