package de.invesdwin.scripting.julia.runtime.juliacaller;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.julia.runtime.juliacaller.pool.ExtendedJuliaCaller;
import de.invesdwin.scripting.julia.runtime.juliacaller.pool.JuliaCallerObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class JuliaCallerScriptTaskEngineJulia implements IScriptTaskEngine {

    private ExtendedJuliaCaller juliaCaller;
    private final JuliaCallerScriptTaskInputsJulia inputs;
    private final JuliaCallerScriptTaskResultsJulia results;

    public JuliaCallerScriptTaskEngineJulia(final ExtendedJuliaCaller juliaCaller) {
        this.juliaCaller = juliaCaller;
        this.inputs = new JuliaCallerScriptTaskInputsJulia(this);
        this.results = new JuliaCallerScriptTaskResultsJulia(this);
    }

    @Override
    public void eval(final String expression) {
        try {
            juliaCaller.execute(expression);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JuliaCallerScriptTaskInputsJulia getInputs() {
        return inputs;
    }

    @Override
    public JuliaCallerScriptTaskResultsJulia getResults() {
        return results;
    }

    @Override
    public void close() {
        juliaCaller = null;
    }

    @Override
    public ExtendedJuliaCaller unwrap() {
        return juliaCaller;
    }

    /**
     * Each instance has its own engine, so no shared locking required.
     */
    @Override
    public ILock getSharedLock() {
        return DisabledLock.INSTANCE;
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static JuliaCallerScriptTaskEngineJulia newInstance() {
        return new JuliaCallerScriptTaskEngineJulia(JuliaCallerObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedJuliaCaller unwrap = unwrap();
                if (unwrap != null) {
                    JuliaCallerObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
