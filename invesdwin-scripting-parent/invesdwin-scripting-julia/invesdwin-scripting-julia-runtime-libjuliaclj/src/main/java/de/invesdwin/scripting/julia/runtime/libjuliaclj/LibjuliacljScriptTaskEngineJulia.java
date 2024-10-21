package de.invesdwin.scripting.julia.runtime.libjuliaclj;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.julia.runtime.libjuliaclj.internal.ExecutorJuliaEngineWrapper;
import de.invesdwin.scripting.julia.runtime.libjuliaclj.internal.IJuliaEngineWrapper;
import de.invesdwin.scripting.julia.runtime.libjuliaclj.internal.UncheckedJuliaEngineWrapper;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;

@NotThreadSafe
public class LibjuliacljScriptTaskEngineJulia implements IScriptTaskEngine {

    private IJuliaEngineWrapper juliaEngine;
    private final LibjuliacljScriptTaskInputsJulia inputs;
    private final LibjuliacljScriptTaskResultsJulia results;

    public LibjuliacljScriptTaskEngineJulia(final IJuliaEngineWrapper juliaEngine) {
        this.juliaEngine = juliaEngine;
        this.inputs = new LibjuliacljScriptTaskInputsJulia(this);
        this.results = new LibjuliacljScriptTaskResultsJulia(this);
    }

    @Override
    public void eval(final String expression) {
        juliaEngine.eval(expression);
    }

    @Override
    public LibjuliacljScriptTaskInputsJulia getInputs() {
        return inputs;
    }

    @Override
    public LibjuliacljScriptTaskResultsJulia getResults() {
        return results;
    }

    @Override
    public void close() {
        if (juliaEngine != null) {
            juliaEngine.reset();
            juliaEngine = null;
        }
    }

    @Override
    public IJuliaEngineWrapper unwrap() {
        return juliaEngine;
    }

    /**
     * Each instance has its own engine, so no shared locking required.
     */
    @Override
    public ILock getSharedLock() {
        return juliaEngine.getLock();
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return UncheckedJuliaEngineWrapper.EXECUTOR;
    }

    public static LibjuliacljScriptTaskEngineJulia newInstance() {
        return new LibjuliacljScriptTaskEngineJulia(ExecutorJuliaEngineWrapper.getInstance());
    }

}
