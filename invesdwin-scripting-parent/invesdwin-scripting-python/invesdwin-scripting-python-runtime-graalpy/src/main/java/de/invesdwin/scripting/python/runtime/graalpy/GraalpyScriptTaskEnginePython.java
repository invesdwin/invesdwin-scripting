package de.invesdwin.scripting.python.runtime.graalpy;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.python.runtime.graalpy.pool.GraalpyScriptEngineObjectPool;
import de.invesdwin.scripting.python.runtime.graalpy.pool.WrappedGraalpyScriptEngine;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class GraalpyScriptTaskEnginePython implements IScriptTaskEngine {

    private WrappedGraalpyScriptEngine pyScriptEngine;
    private final GraalpyScriptTaskInputsPython inputs;
    private final GraalpyScriptTaskResultsPython results;

    public GraalpyScriptTaskEnginePython(final WrappedGraalpyScriptEngine pyScriptEngine) {
        this.pyScriptEngine = pyScriptEngine;
        this.inputs = new GraalpyScriptTaskInputsPython(this);
        this.results = new GraalpyScriptTaskResultsPython(this);
    }

    /**
     * https://github.com/mrj0/pyScriptEngine/issues/55
     */
    @Override
    public void eval(final String expression) {
        pyScriptEngine.eval(expression);
    }

    @Override
    public GraalpyScriptTaskInputsPython getInputs() {
        return inputs;
    }

    @Override
    public GraalpyScriptTaskResultsPython getResults() {
        return results;
    }

    @Override
    public void close() {
        pyScriptEngine = null;
    }

    @Override
    public WrappedGraalpyScriptEngine unwrap() {
        return pyScriptEngine;
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

    public static GraalpyScriptTaskEnginePython newInstance() {
        return new GraalpyScriptTaskEnginePython(GraalpyScriptEngineObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final WrappedGraalpyScriptEngine unwrap = unwrap();
                if (unwrap != null) {
                    GraalpyScriptEngineObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
