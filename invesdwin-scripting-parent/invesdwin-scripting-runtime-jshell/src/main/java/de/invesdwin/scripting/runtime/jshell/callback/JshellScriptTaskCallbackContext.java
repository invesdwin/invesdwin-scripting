package de.invesdwin.scripting.runtime.jshell.callback;

import java.io.Closeable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ObjectScriptTaskParameters;
import de.invesdwin.scripting.callback.ObjectScriptTaskParametersPool;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturns;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnsPool;
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class JshellScriptTaskCallbackContext implements Closeable {

    private static final Map<String, JshellScriptTaskCallbackContext> UUID_CONTEXT = new ConcurrentHashMap<>();

    private final String uuid;
    private final IScriptTaskCallback callback;

    public JshellScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static JshellScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final IScriptTaskEngine engine) {
        engine.getInputs().putString("jshellScriptTaskCallbackContextUuid", getUuid());
        engine.eval(new ClassPathResource(JshellScriptTaskCallbackContext.class.getSimpleName() + ".jsh",
                JshellScriptTaskCallbackContext.class));
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
