package de.invesdwin.context.r.runtime.rserve;

import javax.annotation.concurrent.NotThreadSafe;

import org.rosuda.REngine.REXP;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.r.runtime.rserve.pool.ExtendedRserveSession;
import de.invesdwin.context.r.runtime.rserve.pool.RsessionObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class RserveScriptTaskEngineR implements IScriptTaskEngine {

    private ExtendedRserveSession rsession;
    private final RserveScriptTaskInputsR inputs;
    private final RserveScriptTaskResultsR results;

    public RserveScriptTaskEngineR(final ExtendedRserveSession rsession) {
        this.rsession = rsession;
        this.inputs = new RserveScriptTaskInputsR(this);
        this.results = new RserveScriptTaskResultsR(this);
    }

    @Override
    public void eval(final String expression) {
        final REXP eval = rsession.rawEval(expression);
        if (eval == null) {
            final String error = de.invesdwin.context.r.runtime.rserve.pool.internal.RsessionLogger.get(rsession)
                    .getErrorMessage();
            if (error != null) {
                throw new IllegalStateException(String.valueOf(error));
            }
        }
    }

    @Override
    public RserveScriptTaskInputsR getInputs() {
        return inputs;
    }

    @Override
    public RserveScriptTaskResultsR getResults() {
        return results;
    }

    @Override
    public void close() {
        rsession = null;
    }

    @Override
    public ExtendedRserveSession unwrap() {
        return rsession;
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

    public static RserveScriptTaskEngineR newInstance() {
        return new RserveScriptTaskEngineR(RsessionObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final ExtendedRserveSession unwrap = unwrap();
                if (unwrap != null) {
                    RsessionObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
