package de.invesdwin.scripting.haskell.runtime.ghci.callback.file;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import javax.annotation.concurrent.ThreadSafe;

import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.marshaller.MarshallerYamlJackson;
import de.invesdwin.context.log.error.Err;
import de.invesdwin.context.log.error.LoggedRuntimeException;
import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.haskell.runtime.contract.callback.ScriptTaskParametersHaskellFromJson;
import de.invesdwin.scripting.haskell.runtime.contract.callback.ScriptTaskParametersHaskellFromJsonPool;
import de.invesdwin.scripting.haskell.runtime.contract.callback.ScriptTaskReturnsHaskellToExpression;
import de.invesdwin.scripting.haskell.runtime.contract.callback.ScriptTaskReturnsHaskellToExpressionPool;
import de.invesdwin.util.concurrent.Executors;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.UUIDs;
import de.invesdwin.util.lang.string.Strings;

@ThreadSafe
public class FileScriptTaskCallbackContext implements Closeable {

    private static final File DIRECTORY = new File(ContextProperties.TEMP_DIRECTORY,
            FileScriptTaskCallbackContext.class.getSimpleName());

    private final String uuid;
    private final IScriptTaskCallback callback;
    private final ObjectMapper mapper;
    private final File requestFile;
    private final File responseFile;
    private final File requestPartFile;
    private final File responsePartFile;
    private final WrappedExecutorService handlerExecutor;

    public FileScriptTaskCallbackContext(final IScriptTaskCallback callback) {
        this.uuid = UUIDs.newPseudoRandomUUID();
        this.callback = callback;
        this.mapper = MarshallerYamlJackson.getInstance().getYamlMapper();
        try {
            Files.forceMkdir(DIRECTORY);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        this.requestFile = new File(DIRECTORY, uuid + ".request");
        this.responseFile = new File(DIRECTORY, uuid + ".response");
        this.requestPartFile = new File(DIRECTORY, uuid + ".request.part");
        this.responsePartFile = new File(DIRECTORY, uuid + ".response.part");
        Files.deleteQuietly(requestFile);
        Files.deleteQuietly(responseFile);
        Files.deleteQuietly(requestPartFile);
        Files.deleteQuietly(responsePartFile);
        this.handlerExecutor = Executors
                .newFixedThreadPool(FileScriptTaskCallbackContext.class.getSimpleName() + "_" + uuid, 1);
        this.handlerExecutor.execute(new FileScriptTaskCallbackServerHandler(this));

    }

    public void init(final IScriptTaskEngine engine) {
        final ClassPathResource resource = new ClassPathResource(
                FileScriptTaskCallbackContext.class.getSimpleName() + ".hs", FileScriptTaskCallbackContext.class);
        try (InputStream in = resource.getInputStream()) {
            String script = IOUtils.toString(in, Charset.defaultCharset());
            script = script.replace("{SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE}",
                    "\"" + getRequestPartFile().getAbsolutePath() + "\"");
            script = script.replace("{SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE}",
                    "\"" + getRequestFile().getAbsolutePath() + "\"");
            script = script.replace("{SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE}",
                    "\"" + getResponseFile().getAbsolutePath() + "\"");
            engine.eval(script);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    public String getUuid() {
        return uuid;
    }

    public File getRequestFile() {
        return requestFile;
    }

    public File getResponseFile() {
        return responseFile;
    }

    public File getRequestPartFile() {
        return requestPartFile;
    }

    public File getResponsePartFile() {
        return responsePartFile;
    }

    public String invoke(final String args) {
        final ScriptTaskParametersHaskellFromJson parameters = ScriptTaskParametersHaskellFromJsonPool.INSTANCE
                .borrowObject();
        final ScriptTaskReturnsHaskellToExpression returns = ScriptTaskReturnsHaskellToExpressionPool.INSTANCE
                .borrowObject();
        try {
            final JsonNode jsonArgs = toJsonNode(args);
            parameters.setParameters(jsonArgs);
            final String methodName = parameters.getString(-1);
            callback.invoke(methodName, parameters, returns);
            return returns.getReturnExpression();
        } catch (final Throwable t) {
            final LoggedRuntimeException loggedError = Err.process(t);
            final String errorMessage = Strings.normalizeNewlines(Throwables.concatMessages(loggedError))
                    .replace("\n", " ")
                    .replace("\"", "\\\"");
            returns.returnExpression("error('CallbackException: " + errorMessage + "')");
            return returns.getReturnExpression();
        } finally {
            ScriptTaskReturnsHaskellToExpressionPool.INSTANCE.returnObject(returns);
            ScriptTaskParametersHaskellFromJsonPool.INSTANCE.returnObject(parameters);
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
        handlerExecutor.shutdownNow();
        Files.deleteQuietly(requestFile);
        Files.deleteQuietly(responseFile);
        Files.deleteQuietly(requestPartFile);
        Files.deleteQuietly(responsePartFile);
    }

}
