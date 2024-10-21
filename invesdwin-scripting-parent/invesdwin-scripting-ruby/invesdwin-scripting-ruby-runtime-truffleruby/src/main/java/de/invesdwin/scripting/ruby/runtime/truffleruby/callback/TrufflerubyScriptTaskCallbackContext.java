package de.invesdwin.scripting.ruby.runtime.truffleruby.callback;

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
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class TrufflerubyScriptTaskCallbackContext implements Closeable {

    private static final Map<String, TrufflerubyScriptTaskCallbackContext> UUID_CONTEXT = new ConcurrentHashMap<>();

    private final String uuid;
    private final IScriptTaskCallback callback;

    public TrufflerubyScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static TrufflerubyScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final IScriptTaskEngine engine) {
        engine.getInputs().putString("trufflerubyScriptTaskCallbackContextUuid", getUuid());
        engine.eval(new ClassPathResource(TrufflerubyScriptTaskCallbackContext.class.getSimpleName() + ".rb",
                TrufflerubyScriptTaskCallbackContext.class));
    }

    public String getUuid() {
        return uuid;
    }

    public ObjectScriptTaskReturnValue invoke(final String methodName, final Object... args) {
        final ObjectScriptTaskParameters parameters = ObjectScriptTaskParametersPool.INSTANCE.borrowObject();
        final TrufflerubyScriptTaskReturns returns = TrufflerubyScriptTaskReturnsPool.INSTANCE.borrowObject();
        try {
            parameters.setParameters(args);
            callback.invoke(methodName, parameters, returns);
            return returns.newReturn();
        } finally {
            TrufflerubyScriptTaskReturnsPool.INSTANCE.returnObject(returns);
            ObjectScriptTaskParametersPool.INSTANCE.returnObject(parameters);
        }
    }

    @Override
    public void close() {
        UUID_CONTEXT.remove(uuid);
    }

}
