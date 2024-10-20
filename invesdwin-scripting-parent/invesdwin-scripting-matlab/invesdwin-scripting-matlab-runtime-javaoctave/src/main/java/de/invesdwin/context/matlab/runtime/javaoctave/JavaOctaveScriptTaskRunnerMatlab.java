package de.invesdwin.context.matlab.runtime.javaoctave;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.javaoctave.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.matlab.runtime.javaoctave.pool.OctaveEngineObjectPool;
import de.invesdwin.util.error.Throwables;
import dk.ange.octave.OctaveEngine;
import jakarta.inject.Named;

@Immutable
@Named
public final class JavaOctaveScriptTaskRunnerMatlab
        implements IScriptTaskRunnerMatlab, FactoryBean<JavaOctaveScriptTaskRunnerMatlab> {

    public static final JavaOctaveScriptTaskRunnerMatlab INSTANCE = new JavaOctaveScriptTaskRunnerMatlab();

    public static final String INTERNAL_RESULT_VARIABLE = JavaOctaveScriptTaskRunnerMatlab.class.getSimpleName()
            + "_result";

    /**
     * public for ServiceLoader support
     */
    public JavaOctaveScriptTaskRunnerMatlab() {}

    @Override
    public <T> T run(final AScriptTaskMatlab<T> scriptTask) {
        //get session
        final OctaveEngine octaveEngine = OctaveEngineObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final JavaOctaveScriptTaskEngineMatlab engine = new JavaOctaveScriptTaskEngineMatlab(octaveEngine);
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
            OctaveEngineObjectPool.INSTANCE.returnObject(octaveEngine);
            return result;
        } catch (final Throwable t) {
            OctaveEngineObjectPool.INSTANCE.invalidateObject(octaveEngine);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public JavaOctaveScriptTaskRunnerMatlab getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JavaOctaveScriptTaskRunnerMatlab.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
