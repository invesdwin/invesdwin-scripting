package de.invesdwin.context.julia.runtime.jajub;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.julia.runtime.jajub.pool.ExtendedJuliaBridge;
import de.invesdwin.context.julia.runtime.jajub.pool.JajubObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class JajubScriptTaskEngineJulia implements IScriptTaskEngine {

    private ExtendedJuliaBridge bridge;
    private final JajubScriptTaskInputsJulia inputs;
    private final JajubScriptTaskResultsJulia results;

    public JajubScriptTaskEngineJulia(final ExtendedJuliaBridge bridge) {
        this.bridge = bridge;
        this.inputs = new JajubScriptTaskInputsJulia(this);
        this.results = new JajubScriptTaskResultsJulia(this);
    }

    @Override
    public void eval(final String expression) {
        bridge.eval(expression);
    }

    @Override
    public JajubScriptTaskInputsJulia getInputs() {
        return inputs;
    }

    @Override
    public JajubScriptTaskResultsJulia getResults() {
        return results;
    }

    @Override
    public void close() {
        bridge = null;
    }

    @Override
    public ExtendedJuliaBridge unwrap() {
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

    public static JajubScriptTaskEngineJulia newInstance() {
        return new JajubScriptTaskEngineJulia(JajubObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedJuliaBridge unwrap = unwrap();
                if (unwrap != null) {
                    JajubObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
