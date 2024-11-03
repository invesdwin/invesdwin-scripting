package de.invesdwin.scripting.rust.runtime.rhai;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.rust.runtime.rhai.pool.ExtendedRhaiBridge;
import de.invesdwin.scripting.rust.runtime.rhai.pool.RhaiObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class RhaiScriptTaskEngineRust implements IScriptTaskEngine {

    private ExtendedRhaiBridge bridge;
    private final RhaiScriptTaskInputsRust inputs;
    private final RhaiScriptTaskResultsRust results;

    public RhaiScriptTaskEngineRust(final ExtendedRhaiBridge bridge) {
        this.bridge = bridge;
        this.inputs = new RhaiScriptTaskInputsRust(this);
        this.results = new RhaiScriptTaskResultsRust(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public RhaiScriptTaskInputsRust getInputs() {
        return inputs;
    }

    @Override
    public RhaiScriptTaskResultsRust getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedRhaiBridge unwrap() {
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

    public static RhaiScriptTaskEngineRust newInstance() {
        return new RhaiScriptTaskEngineRust(RhaiObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedRhaiBridge unwrap = unwrap();
                if (unwrap != null) {
                    RhaiObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
