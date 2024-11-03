package de.invesdwin.scripting.rust.runtime.contract.callback.socket;

import java.io.Closeable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.context.log.error.Err;
import de.invesdwin.context.log.error.LoggedRuntimeException;
import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.rust.runtime.contract.callback.ScriptTaskParametersRustFromJson;
import de.invesdwin.scripting.rust.runtime.contract.callback.ScriptTaskParametersRustFromJsonPool;
import de.invesdwin.scripting.rust.runtime.contract.callback.ScriptTaskReturnsRustToExpression;
import de.invesdwin.scripting.rust.runtime.contract.callback.ScriptTaskReturnsRustToExpressionPool;
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
        engine.eval(new ClassPathResource(SocketScriptTaskCallbackContext.class.getSimpleName() + ".py",
                SocketScriptTaskCallbackContext.class));
    }

    public void deinit(final IScriptTaskEngine engine) {
        engine.eval("socketScriptTaskCallbackSocket.shutdown(2); socketScriptTaskCallbackSocket.close()");
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

    public String invoke(final String methodName, final String args) {
        final ScriptTaskParametersRustFromJson parameters = ScriptTaskParametersRustFromJsonPool.INSTANCE
                .borrowObject();
        final ScriptTaskReturnsRustToExpression returns = ScriptTaskReturnsRustToExpressionPool.INSTANCE
                .borrowObject();
        try {
            final JsonNode jsonArgs = toJsonNode(args);
            parameters.setParameters(jsonArgs);
            callback.invoke(methodName, parameters, returns);
            return returns.getReturnExpression();
        } catch (final Throwable t) {
            final LoggedRuntimeException loggedError = Err.process(t);
            final String errorMessage = Strings.normalizeNewlines(Throwables.concatMessages(loggedError))
                    .replace("\n", "\\n")
                    .replace("\"", "\\\"");
            returns.returnExpression("raise Exception(\"CallbackException: " + errorMessage + "\")");
            return returns.getReturnExpression();
        } finally {
            ScriptTaskReturnsRustToExpressionPool.INSTANCE.returnObject(returns);
            ScriptTaskParametersRustFromJsonPool.INSTANCE.returnObject(parameters);
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
