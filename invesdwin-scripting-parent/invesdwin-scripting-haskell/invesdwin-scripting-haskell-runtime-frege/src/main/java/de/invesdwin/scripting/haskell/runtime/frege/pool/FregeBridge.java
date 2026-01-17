package de.invesdwin.scripting.haskell.runtime.frege.pool;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.concurrent.pool.IObjectPool;

@NotThreadSafe
@SuppressWarnings("rawtypes")
public class FregeBridge implements IFregeBridge {

    private final IObjectPool pool;
    private final IFregeBridge delegate;

    public FregeBridge(final IObjectPool<? extends IFregeBridge> pool, final IFregeBridge delegate) {
        this.pool = pool;
        this.delegate = delegate;
    }

    public IObjectPool getPool() {
        return pool;
    }

    public IFregeBridge getDelegate() {
        return delegate;
    }

    @Override
    public void eval(final String expression) {
        delegate.eval(expression);
    }

    @Override
    public JsonNode getAsJsonNode(final String variable) {
        return delegate.getAsJsonNode(variable);
    }

}
