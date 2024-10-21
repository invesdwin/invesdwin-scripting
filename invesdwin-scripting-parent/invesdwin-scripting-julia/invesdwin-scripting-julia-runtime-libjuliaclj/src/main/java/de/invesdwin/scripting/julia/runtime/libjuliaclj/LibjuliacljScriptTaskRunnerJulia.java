package de.invesdwin.scripting.julia.runtime.libjuliaclj;

import java.util.concurrent.Future;

import javax.annotation.concurrent.Immutable;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.LoggingDelegateScriptTaskCallback;
import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskJulia;
import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.scripting.julia.runtime.contract.callback.socket.SocketScriptTaskCallbackContext;
import de.invesdwin.scripting.julia.runtime.libjuliaclj.internal.InitializingJuliaEngineWrapper;
import de.invesdwin.util.concurrent.future.Futures;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.error.Throwables;
import jakarta.inject.Named;

@Immutable
@Named
public final class LibjuliacljScriptTaskRunnerJulia
        implements IScriptTaskRunnerJulia, FactoryBean<LibjuliacljScriptTaskRunnerJulia> {

    public static final LibjuliacljScriptTaskRunnerJulia INSTANCE = new LibjuliacljScriptTaskRunnerJulia();

    /**
     * public for ServiceLoader support
     */
    public LibjuliacljScriptTaskRunnerJulia() {}

    @Override
    public <T> T run(final AScriptTaskJulia<T> scriptTask) {
        //get session
        final LibjuliacljScriptTaskEngineJulia engine = new LibjuliacljScriptTaskEngineJulia(
                InitializingJuliaEngineWrapper.getInstance());
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
    public LibjuliacljScriptTaskRunnerJulia getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return LibjuliacljScriptTaskRunnerJulia.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
