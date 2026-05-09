package de.invesdwin.scripting.runtime.lua.pool;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.util.assertions.Assertions;
import party.iroiro.luajava.value.LuaValue;

@NotThreadSafe
public class LuaScriptEngineFactoryTest extends ATest {

    @Test
    public void test() throws ScriptException {
        final LuaScriptEngineFactory factory = new LuaScriptEngineFactory();
        final LuaScriptEngine engine = factory.getScriptEngine();
        engine.eval("a = 1");
        final LuaValue[] value = engine.eval("return a");
        Assertions.checkEquals(1, value[0].toInteger());

        final LuaValue[] valueNil = engine.eval("__ret__ = a; a = nil; return __ret__");
        Assertions.checkEquals(1, valueNil[0].toInteger());

        final LuaValue[] valueNilled = engine.eval("return a");
        Assertions.checkNull(valueNilled[0].toJavaObject());

        engine.put("hello", "World");
        engine.eval("world = \"Hello \" .. hello .. \"!\"");
        final LuaValue[] world = engine.get("world");
        Assertions.checkEquals("Hello World!", world[0].toJavaObject());
    }

}
