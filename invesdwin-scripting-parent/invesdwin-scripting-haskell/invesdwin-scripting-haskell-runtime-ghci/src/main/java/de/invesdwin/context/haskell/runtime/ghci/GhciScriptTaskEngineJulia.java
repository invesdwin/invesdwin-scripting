package de.invesdwin.context.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.haskell.runtime.ghci.pool.ExtendedGhciBridge;
import de.invesdwin.context.haskell.runtime.ghci.pool.GhciObjectPool;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class GhciScriptTaskEngineJulia implements IScriptTaskEngine {

    private ExtendedGhciBridge bridge;
    private final GhciScriptTaskInputsJulia inputs;
    private final GhciScriptTaskResultsJulia results;

    public GhciScriptTaskEngineJulia(final ExtendedGhciBridge bridge) {
        this.bridge = bridge;
        this.inputs = new GhciScriptTaskInputsJulia(this);
        this.results = new GhciScriptTaskResultsJulia(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public GhciScriptTaskInputsJulia getInputs() {
        return inputs;
    }

    @Override
    public GhciScriptTaskResultsJulia getResults() {
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

    public static GhciScriptTaskEngineJulia newInstance() {
        return new GhciScriptTaskEngineJulia(GhciObjectPool.INSTANCE.borrowObject()) {
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
