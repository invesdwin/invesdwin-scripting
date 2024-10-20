package de.invesdwin.context.matlab.runtime.javaoctave.pool;

import java.io.File;
import java.io.OutputStreamWriter;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;
import org.zeroturnaround.exec.stream.slf4j.Slf4jDebugOutputStream;
import org.zeroturnaround.exec.stream.slf4j.Slf4jWarnOutputStream;

import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.javaoctave.JavaOctaveProperties;
import de.invesdwin.context.matlab.runtime.javaoctave.JavaOctaveScriptTaskEngineMatlab;
import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import dk.ange.octave.OctaveEngine;
import dk.ange.octave.OctaveEngineFactory;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class OctaveEngineObjectPool extends ATimeoutObjectPool<OctaveEngine>
        implements FactoryBean<OctaveEngineObjectPool> {

    public static final OctaveEngineObjectPool INSTANCE = new OctaveEngineObjectPool();

    private final JavaOctaveScriptTaskEngineMatlab reusableEngine = new JavaOctaveScriptTaskEngineMatlab(null);

    private OctaveEngineObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    protected OctaveEngine newObject() {
        final OctaveEngineFactory factory = new OctaveEngineFactory();
        factory.setErrorWriter(new OutputStreamWriter(new Slf4jWarnOutputStream(IScriptTaskRunnerMatlab.LOG)));
        if (JavaOctaveProperties.OCTAVE_COMMAND != null) {
            factory.setOctaveProgram(new File(JavaOctaveProperties.OCTAVE_COMMAND));
        }
        final OctaveEngine scriptEngine = factory.getScriptEngine();
        scriptEngine.setWriter(new OutputStreamWriter(new Slf4jDebugOutputStream(IScriptTaskRunnerMatlab.LOG)));
        return scriptEngine;
    }

    @Override
    protected boolean passivateObject(final OctaveEngine element) {
        reusableEngine.setOctaveEngine(element);
        reusableEngine.eval(IScriptTaskRunnerMatlab.CLEANUP_SCRIPT);
        reusableEngine.close();
        return true;
    }

    @Override
    public void invalidateObject(final OctaveEngine element) {
        try {
            element.close();
        } catch (final Throwable t) {
            //ignore
        }
    }

    @Override
    public OctaveEngineObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return OctaveEngineObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }
}
