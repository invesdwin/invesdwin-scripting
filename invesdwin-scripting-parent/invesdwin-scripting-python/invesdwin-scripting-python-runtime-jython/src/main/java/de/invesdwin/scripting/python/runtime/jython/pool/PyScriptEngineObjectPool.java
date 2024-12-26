package de.invesdwin.scripting.python.runtime.jython.pool;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class PyScriptEngineObjectPool extends ATimeoutObjectPool<WrappedPyScriptEngine>
        implements FactoryBean<PyScriptEngineObjectPool> {

    public static final PyScriptEngineObjectPool INSTANCE = new PyScriptEngineObjectPool();

    private PyScriptEngineObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final WrappedPyScriptEngine element) {
        element.close();
    }

    @Override
    protected WrappedPyScriptEngine newObject() {
        return new WrappedPyScriptEngine();
    }

    /**
     * https://github.com/mrj0/jep/wiki/Performance-Considerations
     * 
     * https://github.com/spyder-ide/spyder/issues/2563
     * 
     * http://stackoverflow.com/questions/3543833/how-do-i-clear-all-variables-in-the-middle-of-a-python-script
     */
    @Override
    protected boolean passivateObject(final WrappedPyScriptEngine element) {
        element.reset();
        return true;
    }

    @Override
    public PyScriptEngineObjectPool getObject() {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return PyScriptEngineObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
