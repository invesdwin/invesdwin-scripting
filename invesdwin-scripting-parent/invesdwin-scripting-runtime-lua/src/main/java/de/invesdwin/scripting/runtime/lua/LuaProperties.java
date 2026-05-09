package de.invesdwin.scripting.runtime.lua;

import java.util.List;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;
import de.invesdwin.util.collections.Collections;
import party.iroiro.luajava.util.ClassUtils;

@Immutable
public final class LuaProperties {

    public static final List<String> LUA_CLASSES;
    public static final String LUA_CLASS;

    static {
        final SystemProperties systemProperties = new SystemProperties(LuaProperties.class);
        LUA_CLASSES = Collections.unmodifiableList(systemProperties.getList("LUA_CLASSES"));
        LUA_CLASS = findAvailableLuaClass();
    }

    private LuaProperties() {}

    private static String findAvailableLuaClass() {
        for (int i = 0; i < LUA_CLASSES.size(); i++) {
            final String engine = LUA_CLASSES.get(i);
            try {
                ClassUtils.forName(engine);
                return engine;
            } catch (final ClassNotFoundException ignored) {
            }
        }
        throw new LinkageError("No available Lua class found");
    }

}
