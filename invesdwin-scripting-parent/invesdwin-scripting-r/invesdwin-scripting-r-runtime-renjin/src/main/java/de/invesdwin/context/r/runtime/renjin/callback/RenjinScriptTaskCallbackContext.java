package de.invesdwin.context.r.runtime.renjin.callback;

import java.io.Closeable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.concurrent.ThreadSafe;

import org.renjin.sexp.SEXP;
import org.springframework.core.io.ClassPathResource;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.util.lang.UUIDs;

@ThreadSafe
public class RenjinScriptTaskCallbackContext implements Closeable {

    private static final Map<String, RenjinScriptTaskCallbackContext> UUID_CONTEXT = new ConcurrentHashMap<>();

    private final String uuid;
    private final IScriptTaskCallback callback;

    public RenjinScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
    }

    public static RenjinScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final IScriptTaskEngine engine) {
        engine.getInputs().putString("renjinScriptTaskCallbackContextUuid", getUuid());
        engine.eval(new ClassPathResource(RenjinScriptTaskCallbackContext.class.getSimpleName() + ".R",
                RenjinScriptTaskCallbackContext.class));
    }

    public String getUuid() {
        return uuid;
    }

    public SexpScriptTaskReturnValue invoke(final String methodName, final SEXP args) {
        final RenjinScriptTaskParametersR parameters = RenjinScriptTaskParametersRPool.INSTANCE.borrowObject();
        final RenjinScriptTaskReturnsR returns = RenjinScriptTaskReturnsRPool.INSTANCE.borrowObject();
        try {
            parameters.setParameters(args);
            callback.invoke(methodName, parameters, returns);
            return returns.newReturn();
        } finally {
            RenjinScriptTaskReturnsRPool.INSTANCE.returnObject(returns);
            RenjinScriptTaskParametersRPool.INSTANCE.returnObject(parameters);
        }
    }

    @Override
    public void close() {
        UUID_CONTEXT.remove(uuid);
    }

}
