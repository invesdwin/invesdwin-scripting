package de.invesdwin.scripting.haskell.runtime.frege.callback.file;

import javax.annotation.concurrent.NotThreadSafe;
import javax.script.Bindings;
import javax.script.ScriptContext;

import org.junit.jupiter.api.Test;

import de.invesdwin.context.test.ATest;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.error.Throwables;
import frege.prelude.PreludeBase.WrappedCheckedException;

@NotThreadSafe
public class FregeEvalTest extends ATest {

    @Test
    public void testFregeEval() {
        final FregeEval eval = new FregeEval();
        eval.eval("a = 1");
        eval.eval("b = a + 1");
        final int a = eval.eval("a");
        Assertions.checkEquals(1, a);
        final int b = eval.eval("b");
        Assertions.checkEquals(2, b);

        //multiline statements work with JSR-223 in frege (though :{ ... :} is not supported, but also not really needed)
        eval.eval("world = \"World\"\n" //
                + "helloWorld = \"Hello \" ++ world ++ \"!\"");
        //plain printing to console does not work with JSR-223 in Frege
        eval.eval("putStrLn ( helloWorld )");
        eval.eval("println ( helloWorld )");

        //it only works with this workaround to force evaluation
        eval.eval("IO.performUnsafe ( putStrLn ( helloWorld ) )");
        eval.eval("IO.performUnsafe ( println ( helloWorld ) )");
        final String helloWorld = eval.eval("helloWorld");
        Assertions.checkEquals("Hello World!", helloWorld);

        eval.eval("divByZero x = if x == 0 then error \"Cannot be zero!\" else 100 / x");
        final double divBy5 = eval.eval("divByZero 5");
        Assertions.checkEquals(20, divBy5);

        //exception handling is cumbersome with JSR-223 in frege
        final WrappedCheckedException divBy0Exception = Assertions.assertThrows(WrappedCheckedException.class, () -> {
            eval.eval("divByZero 0");
        });
        Assertions.assertThat(divBy0Exception.getCause().getMessage()).contains("Cannot be zero!");

        eval.eval("import Data.JSON");
        final String aJson = eval.eval("show ( toJSON a )");
        Assertions.checkEquals("1", aJson);

        //binding is not useable with JSR-223 in frege
        final WrappedCheckedException bindingNotUseableException = Assertions
                .assertThrows(WrappedCheckedException.class, () -> {
                    final Bindings bindings = eval.getEngine().getBindings(ScriptContext.ENGINE_SCOPE);
                    bindings.put("c", 3);
                    final int c = eval.eval("c");
                    Assertions.checkEquals(3, c);
                });
        Assertions.checkNotNull(Throwables.isCausedByType(bindingNotUseableException, NoSuchFieldError.class));

    }

}
