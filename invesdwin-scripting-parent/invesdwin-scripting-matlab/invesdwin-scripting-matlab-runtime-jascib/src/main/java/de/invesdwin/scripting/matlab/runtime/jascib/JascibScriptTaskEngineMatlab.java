package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.matlab.runtime.jascib.pool.ExtendedScilabBridge;
import de.invesdwin.scripting.matlab.runtime.jascib.pool.JascibObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class JascibScriptTaskEngineMatlab implements IScriptTaskEngine {

    private ExtendedScilabBridge juliaCaller;
    private final JascibScriptTaskInputsMatlab inputs;
    private final JascibScriptTaskResultsMatlab results;

    public JascibScriptTaskEngineMatlab(final ExtendedScilabBridge juliaCaller) {
        this.juliaCaller = juliaCaller;
        this.inputs = new JascibScriptTaskInputsMatlab(this);
        this.results = new JascibScriptTaskResultsMatlab(this);
    }

    @Override
    public void eval(final String expression) {
        juliaCaller.eval(expression);
    }

    @Override
    public JascibScriptTaskInputsMatlab getInputs() {
        return inputs;
    }

    @Override
    public JascibScriptTaskResultsMatlab getResults() {
        return results;
    }

    @Override
    public void close() {
        juliaCaller = null;
    }

    @Override
    public ExtendedScilabBridge unwrap() {
        return juliaCaller;
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

    public static JascibScriptTaskEngineMatlab newInstance() {
        return new JascibScriptTaskEngineMatlab(JascibObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedScilabBridge unwrap = unwrap();
                if (unwrap != null) {
                    JascibObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
