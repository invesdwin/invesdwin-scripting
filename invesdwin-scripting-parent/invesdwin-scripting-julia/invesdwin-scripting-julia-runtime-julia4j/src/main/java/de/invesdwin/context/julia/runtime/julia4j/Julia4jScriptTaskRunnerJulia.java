package de.invesdwin.context.julia.runtime.julia4j;

import java.util.concurrent.Future;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.integration.script.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.context.julia.runtime.contract.AScriptTaskJulia;
import de.invesdwin.context.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.context.julia.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.context.julia.runtime.julia4j.internal.UnsafeJuliaEngineWrapper;
import de.invesdwin.util.concurrent.future.Futures;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class Julia4jScriptTaskRunnerJulia
        implements IScriptTaskRunnerJulia, FactoryBean<Julia4jScriptTaskRunnerJulia> {

    public static final Julia4jScriptTaskRunnerJulia INSTANCE = new Julia4jScriptTaskRunnerJulia();

    /**
     * public for ServiceLoader support
     */
    public Julia4jScriptTaskRunnerJulia() {}

    @Override
    public <T> T run(final AScriptTaskJulia<T> scriptTask) {
        //get session
        final Julia4jScriptTaskEngineJulia engine = new Julia4jScriptTaskEngineJulia(UnsafeJuliaEngineWrapper.INSTANCE);
        final Future<T> future = engine.getSharedExecutor().submit(() -> {
            final IScriptTaskCallback callback = scriptTask.getCallback();
            final SocketScriptTaskCallbackContext context;
            if (callback != null) {
                context = new SocketScriptTaskCallbackContext(
                        LoggingDelegateScriptTaskCallback.maybeWrap(LOG, callback));
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
        });
        return Futures.getNoInterrupt(future);
    }

    @Override
    public Julia4jScriptTaskRunnerJulia getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return Julia4jScriptTaskRunnerJulia.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
