package de.invesdwin.scripting.haskell.runtime.frege.pool;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.scripting.haskell.runtime.frege.FregeProperties;
import de.invesdwin.scripting.haskell.runtime.frege.pool.jsr223.Jsr223FregeObjectPool;
import de.invesdwin.scripting.haskell.runtime.frege.pool.repl.ReplFregeObjectPool;
import de.invesdwin.util.concurrent.pool.IObjectPool;

@ThreadSafe
public final class FregeObjectPool implements IObjectPool<FregeBridge> {

    public static final FregeObjectPool INSTANCE = new FregeObjectPool();

    private FregeObjectPool() {}

    @Override
    public FregeBridge borrowObject() {
        final IObjectPool<? extends IFregeBridge> pool;
        if (FregeProperties.isForkedReplProcess()) {
            pool = ReplFregeObjectPool.INSTANCE;
        } else {
            pool = Jsr223FregeObjectPool.INSTANCE;
        }
        return new FregeBridge(pool, pool.borrowObject());
    }

    @SuppressWarnings("unchecked")
    @Override
    public void returnObject(final FregeBridge element) {
        element.getPool().returnObject(element.getDelegate());
    }

    @Override
    public void clear() {
        ReplFregeObjectPool.INSTANCE.clear();
        Jsr223FregeObjectPool.INSTANCE.clear();
    }

    @SuppressWarnings("unchecked")
    @Override
    public void invalidateObject(final FregeBridge element) {
        element.getPool().invalidateObject(element.getDelegate());
    }

    @Override
    public int size() {
        return ReplFregeObjectPool.INSTANCE.size() + Jsr223FregeObjectPool.INSTANCE.size();
    }

}
