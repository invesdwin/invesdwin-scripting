package de.invesdwin.scripting.python.runtime.jython;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.python.runtime.jython.pool.PyScriptEngineObjectPool;
import de.invesdwin.scripting.python.runtime.jython.pool.WrappedPyScriptEngine;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;

@NotThreadSafe
public class JythonScriptTaskEnginePython implements IScriptTaskEngine {

    private WrappedPyScriptEngine pyScriptEngine;
    private final JythonScriptTaskInputsPython inputs;
    private final JythonScriptTaskResultsPython results;

    public JythonScriptTaskEnginePython(final WrappedPyScriptEngine pyScriptEngine) {
        this.pyScriptEngine = pyScriptEngine;
        this.inputs = new JythonScriptTaskInputsPython(this);
        this.results = new JythonScriptTaskResultsPython(this);
    }

    /**
     * https://github.com/mrj0/pyScriptEngine/issues/55
     */
    @Override
    public void eval(final String expression) {
        pyScriptEngine.eval(expression);
    }

    @Override
    public JythonScriptTaskInputsPython getInputs() {
        return inputs;
    }

    @Override
    public JythonScriptTaskResultsPython getResults() {
        return results;
    }

    @Override
    public void close() {
        pyScriptEngine = null;
    }

    @Override
    public WrappedPyScriptEngine unwrap() {
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

    public static JythonScriptTaskEnginePython newInstance() {
        return new JythonScriptTaskEnginePython(PyScriptEngineObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final WrappedPyScriptEngine unwrap = unwrap();
                if (unwrap != null) {
                    PyScriptEngineObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
