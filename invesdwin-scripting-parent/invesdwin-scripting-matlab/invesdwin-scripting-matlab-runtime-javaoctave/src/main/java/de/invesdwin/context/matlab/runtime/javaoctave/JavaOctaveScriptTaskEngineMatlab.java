package de.invesdwin.context.matlab.runtime.javaoctave;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.javaoctave.pool.OctaveEngineObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;
import dk.ange.octave.OctaveEngine;

@NotThreadSafe
public class JavaOctaveScriptTaskEngineMatlab implements IScriptTaskEngine {

    private OctaveEngine octaveEngine;
    private final JavaOctaveScriptTaskInputsMatlab inputs;
    private final JavaOctaveScriptTaskResultsMatlab results;
    private String expressionEnding;

    public JavaOctaveScriptTaskEngineMatlab(final OctaveEngine octaveEngine) {
        this.octaveEngine = octaveEngine;
        this.inputs = new JavaOctaveScriptTaskInputsMatlab(this);
        this.results = new JavaOctaveScriptTaskResultsMatlab(this);
        if (IScriptTaskRunnerMatlab.LOG.isDebugEnabled()) {
            expressionEnding = ";";
        } else {
            expressionEnding = "";
        }
    }

    public void setOctaveEngine(final OctaveEngine octaveEngine) {
        this.octaveEngine = octaveEngine;
    }

    @Override
    public void eval(final String expression) {
        octaveEngine.eval(expression + expressionEnding);
    }

    @Override
    public JavaOctaveScriptTaskInputsMatlab getInputs() {
        return inputs;
    }

    @Override
    public JavaOctaveScriptTaskResultsMatlab getResults() {
        return results;
    }

    @Override
    public void close() {
        octaveEngine = null;
    }

    @Override
    public OctaveEngine unwrap() {
        return octaveEngine;
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

    public static JavaOctaveScriptTaskEngineMatlab newInstance() {
        return new JavaOctaveScriptTaskEngineMatlab(OctaveEngineObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final OctaveEngine unwrap = unwrap();
                if (unwrap != null) {
                    OctaveEngineObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
