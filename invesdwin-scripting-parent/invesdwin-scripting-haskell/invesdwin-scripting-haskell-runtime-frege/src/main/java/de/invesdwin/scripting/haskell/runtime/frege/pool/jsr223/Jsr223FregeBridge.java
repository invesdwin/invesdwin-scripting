package de.invesdwin.scripting.haskell.runtime.frege.pool.jsr223;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.scripting.haskell.runtime.frege.callback.file.FregeEval;
import de.invesdwin.scripting.haskell.runtime.frege.pool.IFregeBridge;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.string.Strings;

/**
 * Fork of: https://github.com/org-arl/jajub/issues/2
 */
@NotThreadSafe
public class Jsr223FregeBridge implements IFregeBridge {

    private FregeEval frege;
    private final ObjectMapper mapper;

    ////// public API

    /**
     * Creates a Java-Frege bridge with default settings.
     */
    public Jsr223FregeBridge() {
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    //CHECKSTYLE:OFF
    @Override
    public void finalize() {
        //CHECKSTYLE:ON
        close();
    }

    public boolean isOpen() {
        return frege != null;
    }

    public void open() throws IOException {
        if (isOpen()) {
            return;
        }
        frege = new FregeEval();
        frege.eval(getStartupScript());
    }

    protected static String getStartupScript() {
        final ClassPathResource resource = new ClassPathResource(Jsr223FregeBridge.class.getSimpleName() + ".fr",
                Jsr223FregeBridge.class);
        try (InputStream in = resource.getInputStream()) {
            final String script = IOUtils.toString(in, Charset.defaultCharset());
            return script;
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Stops a running Frege process.
     */
    public void close() {
        if (!isOpen()) {
            return;
        }
        frege = null;
    }

    @SuppressWarnings("unchecked")
    private <T> T exec(final String jcode, final String logMessage, final Object... logArgs) {
        IScriptTaskRunnerHaskell.LOG.debug(logMessage, logArgs);

        final String[] lines = Strings.splitPreserveAllTokens(jcode, "\n");
        Object result = null;
        StringBuilder multiLine = null;
        for (int i = 0; i < lines.length; i++) {
            final String line = lines[i];
            final String lineTrim = line.trim();
            if (":{".equals(lineTrim)) {
                multiLine = new StringBuilder();
                continue;
            }
            if (multiLine != null) {
                if (":}".equals(lineTrim)) {
                    result = frege.eval(multiLine.toString());
                    multiLine = null;
                } else {
                    multiLine.append(line);
                    multiLine.append("\n");
                }
            } else {
                if (Strings.isNotBlank(line)) {
                    result = frege.eval(line);
                }
            }
        }
        return (T) result;
    }

    public JsonNode getAsJsonNode(final String variable) {
        final StringBuilder message = new StringBuilder();
        message.append("IO.performUnsafe ( showJSON ( ");
        message.append(variable);
        message.append(" ) )");

        final String result = exec(message.toString(), "> get %s", variable);
        try {
            final JsonNode node = mapper.readTree(result);
            if (node instanceof TextNode) {
                //Frege does not support Nothing/null, we emulate it with an empty string
                final TextNode cNode = (TextNode) node;
                if (Strings.isBlankOrNullText(cNode.asText())) {
                    return null;
                }
            }
            if (node instanceof NullNode) {
                return null;
            } else {
                return node;
            }
        } catch (final Throwable t) {
            throw Throwables.propagate(t);
        }
    }

    /**
     * Evaluates an expression in Frege.
     *
     * @param jcode
     *            expression to evaluate.
     * @return value of the expression.
     */
    public void eval(final String jcode) {
        exec(jcode, "> exec %s", jcode);
    }

    ////// private stuff

}
