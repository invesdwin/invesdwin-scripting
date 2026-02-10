package de.invesdwin.scripting.haskell.runtime.callback;

import java.util.Map;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.callback.IScriptTaskCallback;
import de.invesdwin.scripting.callback.ReflectiveScriptTaskCallback;
import de.invesdwin.scripting.callback.ReturnExpression;
import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskHaskell;
import de.invesdwin.scripting.haskell.runtime.contract.IScriptTaskRunnerHaskell;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.factory.ILockCollectionFactory;
import de.invesdwin.util.lang.UUIDs;

@NotThreadSafe
public class SimpleCallbackTest {

    private static final Map<String, String> UUID_SECRET = ILockCollectionFactory.getInstance(true).newConcurrentMap();

    private final IScriptTaskRunnerHaskell runner;
    private int voidMethodCalled;

    public SimpleCallbackTest(final IScriptTaskRunnerHaskell runner) {
        this.runner = runner;
    }

    public static String getSecretStatic(final String uuid) {
        return UUID_SECRET.get(uuid);
    }

    public String getSecret(final String uuid) {
        return UUID_SECRET.get(uuid);
    }

    @ReturnExpression
    public String getSecretExpression(final String uuid) {
        return "\"secret\" ++ \"123\"";
    }

    public void voidMethod() {
        voidMethodCalled++;
    }

    public void testSimpleCallback() {
        final String uuid = UUIDs.newPseudoRandomUUID();
        final String secret = "secret123";
        UUID_SECRET.put(uuid, secret);
        try {
            new AScriptTaskHaskell<Void>() {

                @Override
                public IScriptTaskCallback getCallback() {
                    return new ReflectiveScriptTaskCallback(SimpleCallbackTest.this);
                }

                @Override
                public void populateInputs(final IScriptTaskInputs inputs) {
                    inputs.putString("putUuid", uuid);
                }

                @Override
                public void executeScript(final IScriptTaskEngine engine) {
                    engine.eval(new ClassPathResource(SimpleCallbackTest.class.getSimpleName() + ".hs",
                            SimpleCallbackTest.class));
                }

                @Override
                public Void extractResults(final IScriptTaskResults results) {
                    final String getSecretStaticCallback = results.getString("getSecretStaticCallback");
                    Assertions.assertThat(getSecretStaticCallback).isEqualTo(secret);

                    final String getSecretCallback = results.getString("getSecretCallback");
                    Assertions.assertThat(getSecretCallback).isEqualTo(secret);

                    final String getSecretExpressionCallback = results.getString("getSecretExpressionCallback");
                    Assertions.assertThat(getSecretExpressionCallback).isEqualTo(secret);

                    Assertions.assertThat(voidMethodCalled).isEqualTo(1);
                    return null;
                }
            }.run(runner);
        } finally {
            UUID_SECRET.remove(uuid);
        }
    }

}
