package de.invesdwin.context.matlab.runtime.matconsolectl;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.matconsolectl.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.matlab.runtime.matconsolectl.pool.MatlabProxyObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;
import matlabcontrol.MatlabProxy;

@Immutable
@Named
public final class MatConsoleCtlScriptTaskRunnerMatlab
        implements IScriptTaskRunnerMatlab, FactoryBean<MatConsoleCtlScriptTaskRunnerMatlab> {

    public static final MatConsoleCtlScriptTaskRunnerMatlab INSTANCE = new MatConsoleCtlScriptTaskRunnerMatlab();

    public static final String INTERNAL_RESULT_VARIABLE = MatConsoleCtlScriptTaskRunnerMatlab.class.getSimpleName()
            + "_result";

    /**
     * public for ServiceLoader support
     */
    public MatConsoleCtlScriptTaskRunnerMatlab() {}

    @Override
    public <T> T run(final AScriptTaskMatlab<T> scriptTask) {
        //get session
        final MatlabProxy matlabProxy = MatlabProxyObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final SocketScriptTaskCallbackContext context;
        if (callback != null) {
            context = new SocketScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final MatConsoleCtlScriptTaskEngineMatlab engine = new MatConsoleCtlScriptTaskEngineMatlab(matlabProxy);
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
            MatlabProxyObjectPool.INSTANCE.returnObject(matlabProxy);
            return result;
        } catch (final Throwable t) {
            MatlabProxyObjectPool.INSTANCE.invalidateObject(matlabProxy);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public MatConsoleCtlScriptTaskRunnerMatlab getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return MatConsoleCtlScriptTaskRunnerMatlab.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
