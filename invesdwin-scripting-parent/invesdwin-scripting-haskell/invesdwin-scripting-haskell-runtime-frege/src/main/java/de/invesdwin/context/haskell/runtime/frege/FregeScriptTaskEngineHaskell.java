package de.invesdwin.context.haskell.runtime.frege;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.haskell.runtime.frege.pool.ExtendedFregeBridge;
import de.invesdwin.context.haskell.runtime.frege.pool.FregeObjectPool;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class FregeScriptTaskEngineHaskell implements IScriptTaskEngine {

    private ExtendedFregeBridge bridge;
    private final FregeScriptTaskInputsHaskell inputs;
    private final FregeScriptTaskResultsHaskell results;

    public FregeScriptTaskEngineHaskell(final ExtendedFregeBridge bridge) {
        this.bridge = bridge;
        this.inputs = new FregeScriptTaskInputsHaskell(this);
        this.results = new FregeScriptTaskResultsHaskell(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public FregeScriptTaskInputsHaskell getInputs() {
        return inputs;
    }

    @Override
    public FregeScriptTaskResultsHaskell getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedFregeBridge unwrap() {
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

    public static FregeScriptTaskEngineHaskell newInstance() {
        return new FregeScriptTaskEngineHaskell(FregeObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedFregeBridge unwrap = unwrap();
                if (unwrap != null) {
                    FregeObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
