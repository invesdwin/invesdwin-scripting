package de.invesdwin.scripting.graalvm.jsr223;

import java.util.List;

import javax.annotation.concurrent.Immutable;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import org.graalvm.home.Version;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;

import de.invesdwin.scripting.graalvm.jsr223.compiled.ParseAndExecutePolyglotCompiledScript;

/**
 * Source: https://www.graalvm.org/latest/reference-manual/embed-languages/#compatibility-with-jsr-223-scriptengine
 */
@Immutable
public class PolyglotScriptEngineFactory implements ScriptEngineFactory {

    private final String languageId;
    private final Engine polyglotEngine;
    private final Language language;

    public PolyglotScriptEngineFactory(final String languageId) {
        this.languageId = languageId;
        this.polyglotEngine = Engine.newBuilder().build();
        this.language = polyglotEngine.getLanguages().get(languageId);
    }

    public String getLanguageId() {
        return languageId;
    }

    @Override
    public String getEngineName() {
        return language.getImplementationName();
    }

    @Override
    public String getEngineVersion() {
        return Version.getCurrent().toString();
    }

    @Override
    public List<String> getExtensions() {
        return List.of(languageId);
    }

    @Override
    public List<String> getMimeTypes() {
        return List.copyOf(language.getMimeTypes());
    }

    @Override
    public List<String> getNames() {
        return List.of(language.getName(), languageId, language.getImplementationName());
    }

    @Override
    public String getLanguageName() {
        return language.getName();
    }

    @Override
    public String getLanguageVersion() {
        return language.getVersion();
    }

    @Override
    public Object getParameter(final String key) {
        switch (key) {
        case ScriptEngine.ENGINE:
            return getEngineName();
        case ScriptEngine.ENGINE_VERSION:
            return getEngineVersion();
        case ScriptEngine.LANGUAGE:
            return getLanguageName();
        case ScriptEngine.LANGUAGE_VERSION:
            return getLanguageVersion();
        case ScriptEngine.NAME:
            return languageId;
        default:
            return null;
        }
    }

    @Override
    public String getMethodCallSyntax(final String obj, final String m, final String... args) {
        throw new UnsupportedOperationException("Unimplemented method 'getMethodCallSyntax'");
    }

    @Override
    public String getOutputStatement(final String toDisplay) {
        throw new UnsupportedOperationException("Unimplemented method 'getOutputStatement'");
    }

    @Override
    public String getProgram(final String... statements) {
        throw new UnsupportedOperationException("Unimplemented method 'getProgram'");
    }

    @Override
    public PolyglotScriptEngine getScriptEngine() {
        return new PolyglotScriptEngine(this);
    }

    /**
     * You can customize the context by overriding this
     */
    public Context.Builder customizeContextBuilder(final Context.Builder builder) {
        return builder.allowAllAccess(true);
    }

    public Source.Builder customizeSourceBuilder(final Source.Builder builder) {
        //builder.cached(true) is not needed because this is the default
        return builder;
    }

    public CompiledScript newCompiledScript(final Source src, final PolyglotScriptEngine engine)
            throws ScriptException {
        //return new EvalPolyglotCompiledScript(src, engine);
        return new ParseAndExecutePolyglotCompiledScript(src, engine, false);
    }

}
