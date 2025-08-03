package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.scripting.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.scripting.matlab.runtime.jascib.callback.file.FileScriptTaskCallbackContext;
import de.invesdwin.scripting.matlab.runtime.jascib.pool.ExtendedScilabBridge;
import de.invesdwin.scripting.matlab.runtime.jascib.pool.JascibObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JascibScriptTaskRunnerMatlab
        implements IScriptTaskRunnerMatlab, FactoryBean<JascibScriptTaskRunnerMatlab> {

    public static final JascibScriptTaskRunnerMatlab INSTANCE = new JascibScriptTaskRunnerMatlab();

    /**
     * public for ServiceLoader support
     */
    public JascibScriptTaskRunnerMatlab() {}

    @Override
    public <T> T run(final AScriptTaskMatlab<T> scriptTask) {
        // get session
        final ExtendedScilabBridge pythonBridge = JascibObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final FileScriptTaskCallbackContext context;
        if (callback != null) {
            context = new FileScriptTaskCallbackContext(callback);
        } else {
            context = null;
        }
        try {
            // inputs
            final JascibScriptTaskEngineMatlab engine = new JascibScriptTaskEngineMatlab(pythonBridge);
            if (context != null) {
                context.init(engine);
            }
            scriptTask.populateInputs(engine.getInputs());

            // execute
            scriptTask.executeScript(engine);

            // results
            final T result = scriptTask.extractResults(engine.getResults());
            engine.close();

            // return
            JascibObjectPool.INSTANCE.returnObject(pythonBridge);
            return result;
        } catch (final Throwable t) {
            // we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl
            // might get stuck with some inconsistent state
            JascibObjectPool.INSTANCE.invalidateObject(pythonBridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JascibScriptTaskRunnerMatlab getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JascibScriptTaskRunnerMatlab.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
