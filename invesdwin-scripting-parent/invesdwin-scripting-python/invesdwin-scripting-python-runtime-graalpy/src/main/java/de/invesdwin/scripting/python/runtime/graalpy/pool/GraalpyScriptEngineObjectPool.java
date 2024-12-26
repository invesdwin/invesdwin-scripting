package de.invesdwin.scripting.python.runtime.graalpy.pool;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class GraalpyScriptEngineObjectPool extends ATimeoutObjectPool<WrappedGraalpyScriptEngine>
        implements FactoryBean<GraalpyScriptEngineObjectPool> {

    public static final GraalpyScriptEngineObjectPool INSTANCE = new GraalpyScriptEngineObjectPool();

    private GraalpyScriptEngineObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final WrappedGraalpyScriptEngine element) {
        element.close();
    }

    @Override
    protected WrappedGraalpyScriptEngine newObject() {
        return new WrappedGraalpyScriptEngine();
    }

    /**
     * https://github.com/mrj0/jep/wiki/Performance-Considerations
     * 
     * https://github.com/spyder-ide/spyder/issues/2563
     * 
     * http://stackoverflow.com/questions/3543833/how-do-i-clear-all-variables-in-the-middle-of-a-python-script
     */
    @Override
    protected boolean passivateObject(final WrappedGraalpyScriptEngine element) {
        element.reset();
        return true;
    }

    @Override
    public GraalpyScriptEngineObjectPool getObject() {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return GraalpyScriptEngineObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
