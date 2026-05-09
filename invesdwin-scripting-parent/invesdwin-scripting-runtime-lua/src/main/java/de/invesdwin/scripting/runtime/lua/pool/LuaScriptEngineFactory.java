package de.invesdwin.scripting.runtime.lua.pool;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.Immutable;
import javax.script.ScriptEngineFactory;

import de.invesdwin.scripting.runtime.lua.LuaProperties;
import de.invesdwin.util.collections.Collections;
import party.iroiro.luajava.Lua;

/**
 * Adapted from party.iroiro.luajava.jsr223.LuaScriptEngineFactory
 */
@Immutable
public class LuaScriptEngineFactory implements ScriptEngineFactory {
    private static final List<String> MIME_TYPES;
    private static final List<String> EXTENSIONS;
    private static final char[] HEX_LOOKUP = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd',
            'e', 'f', };

    static {
        final ArrayList<String> mimeTypes = new ArrayList<>(2);
        mimeTypes.add("text/x-lua");
        mimeTypes.add("application/x-lua");
        MIME_TYPES = Collections.unmodifiableList(mimeTypes);
        EXTENSIONS = Collections.singletonList("lua");
    }

    private final List<String> names;
    private final String luaClass;

    /**
     * Creates a factory instance
     *
     * <p>
     * This method is used by {@link java.util.ServiceLoader ServiceLoaders} to instantiate engine factories.
     * </p>
     */
    public LuaScriptEngineFactory() {
        this(LuaProperties.LUA_CLASS);
    }

    /**
     * Creates a factory instance
     *
     * @param name
     *            the engine name
     * @param version
     *            the engine version
     * @param luaClass
     *            the fully qualified name of the {@link Lua} implementation class
     */
    public LuaScriptEngineFactory(final String luaClass) {
        this.luaClass = luaClass;

        names = Collections.singletonList(luaClass.toLowerCase().replace(" ", "").replace(".", ""));
    }

    @Override
    public String getEngineName() {
        return "Lua";
    }

    @Override
    public String getEngineVersion() {
        return "1.0";
    }

    @Override
    public List<String> getExtensions() {
        return EXTENSIONS;
    }

    @Override
    public List<String> getMimeTypes() {
        return MIME_TYPES;
    }

    @Override
    public List<String> getNames() {
        return names;
    }

    @Override
    public String getLanguageName() {
        return "Lua";
    }

    @Override
    public String getLanguageVersion() {
        return "unknown";
    }

    @Override
    public Object getParameter(final String s) {
        switch (s) {
        case "ENGINE":
            return getEngineName();
        case "ENGINE_VERSION":
            return getEngineVersion();
        case "LANGUAGE":
            return getLanguageName();
        case "LANGUAGE_VERSION":
            return getLanguageVersion();
        case "NAME":
            return getNames().get(0);
        case "THREADING":
            return "THREAD-ISOLATED";
        default:
            return null;
        }
    }

    @Override
    public String getMethodCallSyntax(final String obj, final String m, final String... args) {
        return obj + ":" + m + "(" + String.join(", ", args) + ")";
    }

    @Override
    public String getOutputStatement(final String s) {
        return escape(s, "print(\"", "\")");
    }

    @Override
    public String getProgram(final String... strings) {
        int length = strings.length * 2;
        for (final String string : strings) {
            length += string.length();
        }
        final StringBuilder builder = new StringBuilder(length);
        for (final String string : strings) {
            builder.append(string).append(";\n");
        }
        return builder.toString();
    }

    @Override
    public LuaScriptEngine getScriptEngine() {
        return new LuaScriptEngine(luaClass, this);
    }

    private static String escape(final String s, final String prefix, final String suffix) {
        final byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
        final StringBuilder builder = new StringBuilder(bytes.length * 4 + prefix.length() + suffix.length());
        builder.append(prefix);
        for (final byte b : bytes) {
            builder.append("\\x");
            builder.append(HEX_LOOKUP[b >> 4]);
            builder.append(HEX_LOOKUP[b & 0x0F]);
        }
        builder.append(suffix);
        return builder.toString();
    }

}
