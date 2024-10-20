package de.invesdwin.context.matlab.runtime.matconsolectl.pool;

import java.io.OutputStreamWriter;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;
import org.zeroturnaround.exec.stream.slf4j.Slf4jDebugOutputStream;
import org.zeroturnaround.exec.stream.slf4j.Slf4jWarnOutputStream;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.matconsolectl.MatConsoleCtlProperties;
import de.invesdwin.context.matlab.runtime.matconsolectl.MatConsoleCtlScriptTaskEngineMatlab;
import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;
import matlabcontrol.MatlabConnectionException;
import matlabcontrol.MatlabInvocationException;
import matlabcontrol.MatlabProxy;
import matlabcontrol.MatlabProxyFactory;
import matlabcontrol.MatlabProxyFactoryOptions;
import matlabcontrol.MatlabProxyFactoryOptions.Builder;

@ThreadSafe
@Named
public final class MatlabProxyObjectPool extends ATimeoutObjectPool<MatlabProxy>
        implements FactoryBean<MatlabProxyObjectPool> {

    public static final MatlabProxyObjectPool INSTANCE = new MatlabProxyObjectPool();

    private final MatConsoleCtlScriptTaskEngineMatlab reusableEngine = new MatConsoleCtlScriptTaskEngineMatlab(null);

    private MatlabProxyObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    protected MatlabProxy newObject() {
        final Builder options = new MatlabProxyFactoryOptions.Builder().setHidden(true);
        if (MatConsoleCtlProperties.MATLAB_COMMAND != null) {
            options.setMatlabLocation(MatConsoleCtlProperties.MATLAB_COMMAND);
        }
        options.setOutputWriter(new OutputStreamWriter(new Slf4jDebugOutputStream(IScriptTaskRunnerMatlab.LOG)));
        options.setErrorWriter(new OutputStreamWriter(new Slf4jWarnOutputStream(IScriptTaskRunnerMatlab.LOG)));
        final MatlabProxyFactory factory = new MatlabProxyFactory(options.build());
        try {
            return factory.getProxy();
        } catch (final MatlabConnectionException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected boolean passivateObject(final MatlabProxy element) {
        reusableEngine.setMatlabProxy(element);
        reusableEngine.eval(IScriptTaskRunnerMatlab.CLEANUP_SCRIPT);
        reusableEngine.close();
        return true;
    }

    @Override
    public void invalidateObject(final MatlabProxy element) {
        try {
            element.exit();
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public MatlabProxyObjectPool getObject() {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return MatlabProxyObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
