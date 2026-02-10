package de.invesdwin.scripting.ruby.runtime.jruby.callback;

import java.io.Closeable;
import java.util.Map;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ObjectScriptTaskParameters;
import de.invesdwin.scripting.callback.ObjectScriptTaskParametersPool;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturns;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnsPool;
import de.invesdwin.util.collections.factory.ILockCollectionFactory;
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class JrubyScriptTaskCallbackContext implements Closeable {

    private static final Map<String, JrubyScriptTaskCallbackContext> UUID_CONTEXT = ILockCollectionFactory
            .getInstance(true)
            .newConcurrentMap();

    private final String uuid;
    private final IScriptTaskCallback callback;

    public JrubyScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static JrubyScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final IScriptTaskEngine engine) {
        engine.getInputs().putString("jrubyScriptTaskCallbackContextUuid", getUuid());
        engine.eval(new ClassPathResource(JrubyScriptTaskCallbackContext.class.getSimpleName() + ".rb",
                JrubyScriptTaskCallbackContext.class));
    }

    public String getUuid() {
        return uuid;
    }

    public ObjectScriptTaskReturnValue invoke(final String methodName, final Object... args) {
        final ObjectScriptTaskParameters parameters = ObjectScriptTaskParametersPool.INSTANCE.borrowObject();
        final ObjectScriptTaskReturns returns = ObjectScriptTaskReturnsPool.INSTANCE.borrowObject();
        try {
            parameters.setParameters(args);
            callback.invoke(methodName, parameters, returns);
            return returns.newReturn();
        } finally {
            ObjectScriptTaskReturnsPool.INSTANCE.returnObject(returns);
            ObjectScriptTaskParametersPool.INSTANCE.returnObject(parameters);
        }
    }

    @Override
    public void close() {
        UUID_CONTEXT.remove(uuid);
    }

}
