package de.invesdwin.context.r.runtime.contract.callback.socket;

import java.io.Closeable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.integration.script.callback.IScriptTaskCallback;
import de.invesdwin.context.log.error.Err;
import de.invesdwin.context.log.error.LoggedRuntimeException;
import de.invesdwin.context.r.runtime.contract.callback.ScriptTaskParametersRFromJson;
import de.invesdwin.context.r.runtime.contract.callback.ScriptTaskParametersRFromJsonPool;
import de.invesdwin.context.r.runtime.contract.callback.ScriptTaskReturnsRToExpression;
import de.invesdwin.context.r.runtime.contract.callback.ScriptTaskReturnsRToExpressionPool;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.UUIDs;
import de.invesdwin.util.lang.string.Strings;

@ThreadSafe
public class SocketScriptTaskCallbackContext implements Closeable {

    private static final Map<String, SocketScriptTaskCallbackContext> UUID_CONTEXT = new ConcurrentHashMap<>();

    private final String uuid;
    private final IScriptTaskCallback callback;
    private final ObjectMapper mapper;
    private final SocketScriptTaskCallbackServer server;

    public SocketScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        UUID_CONTEXT.put(uuid, this);
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
        this.server = SocketScriptTaskCallbackServerPool.INSTANCE.borrowObject();
    }

    public static SocketScriptTaskCallbackContext getContext(final String uuid) {
        return UUID_CONTEXT.get(uuid);
    }

    public void init(final IScriptTaskEngine engine) {
        engine.getInputs().putString("socketScriptTaskCallbackContextUuid", getUuid());
        engine.getInputs().putString("socketScriptTaskCallbackServerHost", getServerHost());
        engine.getInputs().putInteger("socketScriptTaskCallbackServerPort", getServerPort());
        engine.eval(new ClassPathResource(SocketScriptTaskCallbackContext.class.getSimpleName() + ".r",
                SocketScriptTaskCallbackContext.class));
    }

    public void deinit(final IScriptTaskEngine engine) {
        engine.eval("close(socketScriptTaskCallbackSocket)");

    }

    public String getUuid() {
        return uuid;
    }

    public String getServerHost() {
        return server.getHost();
    }

    public int getServerPort() {
        return server.getPort();
    }

    public String invoke(final String methodName, final String dims, final String args) {
        final ScriptTaskParametersRFromJson parameters = ScriptTaskParametersRFromJsonPool.INSTANCE.borrowObject();
        final ScriptTaskReturnsRToExpression returns = ScriptTaskReturnsRToExpressionPool.INSTANCE.borrowObject();
        try {
            final JsonNode jsonDims = toJsonNode(dims);
            final JsonNode jsonArgs = toJsonNode(args);
            parameters.setParameters(jsonDims, jsonArgs);
            callback.invoke(methodName, parameters, returns);
            return returns.getReturnExpression();
        } catch (final Throwable t) {
            final LoggedRuntimeException loggedError = Err.process(t);
            final String errorMessage = Strings.normalizeNewlines(Throwables.concatMessages(loggedError))
                    .replace("\n", "\\n")
                    .replace("\"", "\\\"");
            returns.returnExpression("stop(\"CallbackException: " + errorMessage + "\")");
            return returns.getReturnExpression();
        } finally {
            ScriptTaskReturnsRToExpressionPool.INSTANCE.returnObject(returns);
            ScriptTaskParametersRFromJsonPool.INSTANCE.returnObject(parameters);
        }
    }

    private JsonNode toJsonNode(final String json) {
        try {
            final JsonNode node = mapper.readTree(json);
            if (node instanceof NullNode) {
                return null;
            } else {
                return node;
            }
        } catch (final Throwable t) {
            throw Throwables.propagate(t);
        }
    }

    @Override
    public void close() {
        UUID_CONTEXT.remove(uuid);
        SocketScriptTaskCallbackServerPool.INSTANCE.returnObject(server);
    }

}
