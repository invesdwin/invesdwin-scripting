package de.invesdwin.context.r.runtime.rcaller;

import javax.annotation.concurrent.NotThreadSafe;

import com.github.rcaller.rstuff.RCaller;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.r.runtime.rcaller.pool.RCallerObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class RCallerScriptTaskEngineR implements IScriptTaskEngine {

    private RCaller rcaller;
    private final RCallerScriptTaskInputsR inputs;
    private final RCallerScriptTaskResultsR results;

    public RCallerScriptTaskEngineR(final RCaller rcaller) {
        this.rcaller = rcaller;
        this.inputs = new RCallerScriptTaskInputsR(this);
        this.results = new RCallerScriptTaskResultsR(this);
    }

    @Override
    public void eval(final String expression) {
        rcaller.getRCode().addRCode(expression);
        rcaller.getRCode().addRCode(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE + " <- c()");
        rcaller.runAndReturnResultOnline(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE);
    }

    @Override
    public RCallerScriptTaskInputsR getInputs() {
        return inputs;
    }

    @Override
    public RCallerScriptTaskResultsR getResults() {
        return results;
    }

    @Override
    public void close() {
        rcaller = null;
    }

    @Override
    public RCaller unwrap() {
        return rcaller;
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

    public static RCallerScriptTaskEngineR newInstance() {
        return new RCallerScriptTaskEngineR(RCallerObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final RCaller unwrap = unwrap();
                if (unwrap != null) {
                    RCallerObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
