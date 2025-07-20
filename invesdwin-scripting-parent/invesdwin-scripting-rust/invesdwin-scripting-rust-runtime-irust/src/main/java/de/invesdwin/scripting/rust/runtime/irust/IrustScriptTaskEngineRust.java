package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.rust.runtime.irust.pool.IrustObjectPool;
import de.invesdwin.scripting.rust.runtime.irust.pool.ExtendedIrustBridge;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class IrustScriptTaskEngineRust implements IScriptTaskEngine {

    private ExtendedIrustBridge bridge;
    private final IrustScriptTaskInputsRust inputs;
    private final IrustScriptTaskResultsRust results;

    public IrustScriptTaskEngineRust(final ExtendedIrustBridge bridge) {
        this.bridge = bridge;
        this.inputs = new IrustScriptTaskInputsRust(this);
        this.results = new IrustScriptTaskResultsRust(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public IrustScriptTaskInputsRust getInputs() {
        return inputs;
    }

    @Override
    public IrustScriptTaskResultsRust getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedIrustBridge unwrap() {
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

    public static IrustScriptTaskEngineRust newInstance() {
        return new IrustScriptTaskEngineRust(IrustObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedIrustBridge unwrap = unwrap();
                if (unwrap != null) {
                    IrustObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
