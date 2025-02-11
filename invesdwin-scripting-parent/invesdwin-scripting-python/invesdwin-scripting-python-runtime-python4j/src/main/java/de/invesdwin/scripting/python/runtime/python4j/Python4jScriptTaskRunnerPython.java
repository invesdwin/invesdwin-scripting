package de.invesdwin.scripting.python.runtime.python4j;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.python.runtime.contract.AScriptTaskPython;
import de.invesdwin.scripting.python.runtime.contract.IScriptTaskRunnerPython;
import de.invesdwin.scripting.python.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.python.runtime.python4j.internal.IPythonEngineWrapper;
import de.invesdwin.scripting.python.runtime.python4j.internal.InitializingPythonEngineWrapper;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

/**
 * We have to always use the same thread for accessing the jep instance, thus run the tasks in an executor.
 * 
 * @author subes
 *
 */
@Immutable
@Named
public final class Python4jScriptTaskRunnerPython
        implements IScriptTaskRunnerPython, FactoryBean<Python4jScriptTaskRunnerPython> {

    public static final Python4jScriptTaskRunnerPython INSTANCE = new Python4jScriptTaskRunnerPython();

    /**
     * public for ServiceLoader support
     */
    public Python4jScriptTaskRunnerPython() {}

    @Override
    public <T> T run(final AScriptTaskPython<T> scriptTask) {
        //get session
        final IPythonEngineWrapper pythonEngine = InitializingPythonEngineWrapper.getInstance();
        //inputs
        final Python4jScriptTaskEnginePython engine = new Python4jScriptTaskEnginePython(pythonEngine);
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        final ILock lock = engine.getSharedLock();
        lock.lock();
        try {
            if (context != null) {
                context.init(engine);
            }
            scriptTask.populateInputs(engine.getInputs());

            //execute
            scriptTask.executeScript(engine);

            //results
            final T result = scriptTask.extractResults(engine.getResults());
            if (context != null) {
                context.deinit(engine);
            }
            engine.close();

            //return
            return result;
        } catch (final Throwable t) {
            throw Throwables.propagate(t);
        } finally {
            lock.unlock();
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public Python4jScriptTaskRunnerPython getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return Python4jScriptTaskRunnerPython.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
