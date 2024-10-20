package de.invesdwin.context.matlab.runtime.javasci;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.javasci.callback.file.FileScriptTaskCallbackContext;
import de.invesdwin.context.matlab.runtime.javasci.internal.ScilabWrapper;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class JavasciScriptTaskRunnerMatlab
        implements IScriptTaskRunnerMatlab, FactoryBean<JavasciScriptTaskRunnerMatlab> {

    public static final String CLEANUP_SCRIPT = "clear; clc";
    public static final String INTERNAL_RESULT_VARIABLE = "JSTRM_result";

    public static final JavasciScriptTaskRunnerMatlab INSTANCE = new JavasciScriptTaskRunnerMatlab();

    static {
        Assertions.checkNotNull(JavasciProperties.JAVASCI_LIBRARY_PATHS);
    }

    /**
     * public for ServiceLoader support
     */
    public JavasciScriptTaskRunnerMatlab() {}

    @Override
    public <T> T run(final AScriptTaskMatlab<T> scriptTask) {
        //get session
        final JavasciScriptTaskEngineMatlab engine = new JavasciScriptTaskEngineMatlab(ScilabWrapper.INSTANCE);
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final FileScriptTaskCallbackContext context;
        if (callback != null) {
            context = new FileScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        final ILock lock = engine.getSharedLock();
        lock.lock();
        try {
            //inputs
            if (context != null) {
                context.init(engine);
            }
            scriptTask.populateInputs(engine.getInputs());

            //execute
            scriptTask.executeScript(engine);

            //results
            final T result = scriptTask.extractResults(engine.getResults());
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
    public JavasciScriptTaskRunnerMatlab getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return JavasciScriptTaskRunnerMatlab.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
