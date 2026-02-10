package de.invesdwin.scripting.python.runtime.jep;

import java.io.Closeable;
import java.util.Map;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ObjectScriptTaskParameters;
import de.invesdwin.scripting.callback.ObjectScriptTaskParametersPool;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturns;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnsPool;
import de.invesdwin.util.collections.factory.ILockCollectionFactory;
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class JepScriptTaskCallbackContext implements Closeable {

    private static final Map<String, JepScriptTaskCallbackContext> UUID_CONTEXT = ILockCollectionFactory
            .getInstance(true)
            .newConcurrentMap();

    private final String uuid;
    private final IScriptTaskCallback callback;

    public JepScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static JepScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final JepScriptTaskEnginePython engine) {
        engine.getInputs().putString("jepScriptTaskCallbackContextUuid", getUuid());
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
