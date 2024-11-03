package de.invesdwin.scripting.rust.runtime.evcxr;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.rust.runtime.evcxr.pool.ExtendedEvcxrBridge;
import de.invesdwin.scripting.rust.runtime.evcxr.pool.EvcxrObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class EvcxrScriptTaskEngineRust implements IScriptTaskEngine {

    private ExtendedEvcxrBridge bridge;
    private final EvcxrScriptTaskInputsRust inputs;
    private final EvcxrScriptTaskResultsRust results;

    public EvcxrScriptTaskEngineRust(final ExtendedEvcxrBridge bridge) {
        this.bridge = bridge;
        this.inputs = new EvcxrScriptTaskInputsRust(this);
        this.results = new EvcxrScriptTaskResultsRust(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public EvcxrScriptTaskInputsRust getInputs() {
        return inputs;
    }

    @Override
    public EvcxrScriptTaskResultsRust getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedEvcxrBridge unwrap() {
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

    public static EvcxrScriptTaskEngineRust newInstance() {
        return new EvcxrScriptTaskEngineRust(EvcxrObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedEvcxrBridge unwrap = unwrap();
                if (unwrap != null) {
                    EvcxrObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
