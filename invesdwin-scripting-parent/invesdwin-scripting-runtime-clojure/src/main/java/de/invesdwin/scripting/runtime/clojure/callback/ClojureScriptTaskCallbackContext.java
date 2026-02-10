package de.invesdwin.scripting.runtime.clojure.callback;

import java.io.Closeable;
import java.util.Map;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ObjectScriptTaskParameters;
import de.invesdwin.scripting.callback.ObjectScriptTaskParametersPool;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturns;
import de.invesdwin.scripting.callback.ObjectScriptTaskReturnsPool;
import de.invesdwin.scripting.runtime.clojure.ScriptTaskEngineClojure;
import de.invesdwin.scripting.runtime.clojure.pool.WrappedClojureEngine;
import de.invesdwin.util.collections.factory.ILockCollectionFactory;
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class ClojureScriptTaskCallbackContext implements Closeable {

    private static final Map<String, ClojureScriptTaskCallbackContext> UUID_CONTEXT = ILockCollectionFactory
            .getInstance(true)
            .newConcurrentMap();

    private final String uuid;
    private final IScriptTaskCallback callback;

    private WrappedClojureEngine engine;

    public ClojureScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static ClojureScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final ScriptTaskEngineClojure engine) {
        engine.getInputs().putString("clojureScriptTaskCallbackContextUuid", getUuid());
        engine.eval(new ClassPathResource(ClojureScriptTaskCallbackContext.class.getSimpleName() + ".clj",
                ClojureScriptTaskCallbackContext.class));
        this.engine = engine.unwrap();
    }

    public String getUuid() {
        return uuid;
    }

    public Object invoke(final String methodName, final Object... args) {
        final ObjectScriptTaskParameters parameters = ObjectScriptTaskParametersPool.INSTANCE.borrowObject();
        final ObjectScriptTaskReturns returns = ObjectScriptTaskReturnsPool.INSTANCE.borrowObject();
        try {
            parameters.setParameters(args);
            callback.invoke(methodName, parameters, returns);
            if (returns.isReturnExpression()) {
                return engine.eval((String) returns.getReturnValue());
            } else {
                return returns.getReturnValue();
            }
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
