package de.invesdwin.context.r.runtime.renjin;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.ScriptException;

import org.renjin.script.RenjinScriptEngine;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.r.runtime.renjin.pool.RenjinScriptEngineObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class RenjinScriptTaskEngineR implements IScriptTaskEngine {

    private RenjinScriptEngine renjinScriptEngine;
    private final RenjinScriptTaskInputsR inputs;
    private final RenjinScriptTaskResultsR results;

    public RenjinScriptTaskEngineR(final RenjinScriptEngine renjinScriptEngine) {
        this.renjinScriptEngine = renjinScriptEngine;
        this.inputs = new RenjinScriptTaskInputsR(this);
        this.results = new RenjinScriptTaskResultsR(this);
    }

    @Override
    public void eval(final String expression) {
        try {
            renjinScriptEngine.eval(expression);
        } catch (final ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public RenjinScriptTaskInputsR getInputs() {
        return inputs;
    }

    @Override
    public RenjinScriptTaskResultsR getResults() {
        return results;
    }

    @Override
    public void close() {
        renjinScriptEngine = null;
    }

    @Override
    public RenjinScriptEngine unwrap() {
        return renjinScriptEngine;
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

    public static RenjinScriptTaskEngineR newInstance() {
        return new RenjinScriptTaskEngineR(RenjinScriptEngineObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final RenjinScriptEngine unwrap = unwrap();
                if (unwrap != null) {
                    RenjinScriptEngineObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
