package de.invesdwin.context.matlab.runtime.javasci.callback.file;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.util.concurrent.pool.AAgronaObjectPool;

@ThreadSafe
public final class JavasciScriptTaskReturnsMatlabToExpressionPool
        extends AAgronaObjectPool<JavasciScriptTaskReturnsMatlabToExpression> {

    public static final JavasciScriptTaskReturnsMatlabToExpressionPool INSTANCE = new JavasciScriptTaskReturnsMatlabToExpressionPool();

    private JavasciScriptTaskReturnsMatlabToExpressionPool() {}

    @Override
    protected JavasciScriptTaskReturnsMatlabToExpression newObject() {
        return new JavasciScriptTaskReturnsMatlabToExpression();
    }

    @Override
    protected boolean passivateObject(final JavasciScriptTaskReturnsMatlabToExpression element) {
        element.close();
        return true;
    }

}
