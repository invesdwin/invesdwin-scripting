package de.invesdwin.context.r.runtime.jri;

import javax.annotation.concurrent.NotThreadSafe;

import org.rosuda.JRI.REXP;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.jri.internal.LoggingRMainLoopCallbacks;
import de.invesdwin.context.r.runtime.jri.internal.RengineWrapper;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;

@NotThreadSafe
public class JriScriptTaskEngineR implements IScriptTaskEngine {

    private RengineWrapper rengine;
    private final JriScriptTaskInputsR inputs;
    private final JriScriptTaskResultsR results;

    public JriScriptTaskEngineR(final RengineWrapper rengine) {
        this.rengine = rengine;
        this.inputs = new JriScriptTaskInputsR(this);
        this.results = new JriScriptTaskResultsR(this);
    }

    @Override
    public void eval(final String expression) {
        final REXP eval = rengine.getRengine().eval("eval(parse(text=\"" + expression.replace("\"", "\\\"") + "\"))");
        if (eval == null) {
            throw new IllegalStateException(String.valueOf(LoggingRMainLoopCallbacks.INSTANCE.getErrorMessage()));
        }
    }

    @Override
    public JriScriptTaskInputsR getInputs() {
        return inputs;
    }

    @Override
    public JriScriptTaskResultsR getResults() {
        return results;
    }

    @Override
    public void close() {
        if (rengine != null) {
            eval(IScriptTaskRunnerR.CLEANUP_SCRIPT);
            rengine = null;
        }
    }

    @Override
    public RengineWrapper unwrap() {
        return rengine;
    }

    @Override
    public ILock getSharedLock() {
        return unwrap().getLock();
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static JriScriptTaskEngineR newInstance() {
        return new JriScriptTaskEngineR(RengineWrapper.INSTANCE);
    }

}
