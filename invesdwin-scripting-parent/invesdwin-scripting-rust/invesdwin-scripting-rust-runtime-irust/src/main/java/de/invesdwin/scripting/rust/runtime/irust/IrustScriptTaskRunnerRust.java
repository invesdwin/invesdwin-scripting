package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.rust.runtime.contract.AScriptTaskRust;
import de.invesdwin.scripting.rust.runtime.contract.IScriptTaskRunnerRust;
import de.invesdwin.scripting.rust.runtime.contract.callback.file.FileScriptTaskCallbackContext;
import de.invesdwin.scripting.rust.runtime.irust.pool.ExtendedIrustBridge;
import de.invesdwin.scripting.rust.runtime.irust.pool.IrustObjectPool;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class IrustScriptTaskRunnerRust implements IScriptTaskRunnerRust, FactoryBean<IrustScriptTaskRunnerRust> {

    public static final IrustScriptTaskRunnerRust INSTANCE = new IrustScriptTaskRunnerRust();

    /**
     * public for ServiceLoader support
     */
    public IrustScriptTaskRunnerRust() {}

    @Override
    public <T> T run(final AScriptTaskRust<T> scriptTask) {
        //get session
        final ExtendedIrustBridge bridge = IrustObjectPool.INSTANCE.borrowObject();
        final IScriptTaskCallback callback = scriptTask.getCallback();
        final FileScriptTaskCallbackContext context;
        if (callback != null) {
            context = new FileScriptTaskCallbackContext(LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
        } else {
            context = null;
        }
        try {
            //inputs
            final IrustScriptTaskEngineRust engine = new IrustScriptTaskEngineRust(bridge);
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
            IrustObjectPool.INSTANCE.returnObject(bridge);
            return result;
        } catch (final Throwable t) {
            //we have to destroy instances on exceptions, otherwise e.g. SFrontiers.jl might get stuck with some inconsistent state
            IrustObjectPool.INSTANCE.invalidateObject(bridge);
            throw Throwables.propagate(t);
        } finally {
            if (context != null) {
                context.close();
            }
        }
    }

    @Override
    public IrustScriptTaskRunnerRust getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return IrustScriptTaskRunnerRust.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
