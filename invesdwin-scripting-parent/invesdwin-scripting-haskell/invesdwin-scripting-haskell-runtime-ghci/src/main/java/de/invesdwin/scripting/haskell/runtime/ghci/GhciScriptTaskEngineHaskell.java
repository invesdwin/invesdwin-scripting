package de.invesdwin.scripting.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.ExtendedGhciBridge;
import de.invesdwin.scripting.haskell.runtime.ghci.pool.GhciObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class GhciScriptTaskEngineHaskell implements IScriptTaskEngine {

    private ExtendedGhciBridge bridge;
    private final GhciScriptTaskInputsHaskell inputs;
    private final GhciScriptTaskResultsHaskell results;

    public GhciScriptTaskEngineHaskell(final ExtendedGhciBridge bridge) {
        this.bridge = bridge;
        this.inputs = new GhciScriptTaskInputsHaskell(this);
        this.results = new GhciScriptTaskResultsHaskell(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public GhciScriptTaskInputsHaskell getInputs() {
        return inputs;
    }

    @Override
    public GhciScriptTaskResultsHaskell getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedGhciBridge unwrap() {
        return bridge;
    }

    /**
     * Each instance has its own engine, so no shared locking required.
     */
    @Override
    public ILock getSharedLock() {
        return DisabledLock.INSTANCE;
    }

    /**
     * No executor needed.
     */
    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static GhciScriptTaskEngineHaskell newInstance() {
        return new GhciScriptTaskEngineHaskell(GhciObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedGhciBridge unwrap = unwrap();
                if (unwrap != null) {
                    GhciObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
