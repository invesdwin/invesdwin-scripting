package de.invesdwin.context.r.runtime.renjin.callback;

import javax.annotation.concurrent.ThreadSafe;

import org.renjin.sexp.SEXP;

@ThreadSafe
public class SexpScriptTaskReturnValue {
    private final boolean returnExpression;
    private final SEXP returnValue;

    public SexpScriptTaskReturnValue(final boolean returnExpression, final SEXP returnValue) {
        this.returnExpression = returnExpression;
        this.returnValue = returnValue;
    }

    public SEXP getReturnValue() {
        return returnValue;
    }

    public boolean isReturnExpression() {
        return returnExpression;
    }

}