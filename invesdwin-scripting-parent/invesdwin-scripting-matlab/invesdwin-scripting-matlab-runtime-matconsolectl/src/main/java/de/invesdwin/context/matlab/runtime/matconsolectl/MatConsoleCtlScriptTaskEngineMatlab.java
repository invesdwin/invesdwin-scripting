package de.invesdwin.context.matlab.runtime.matconsolectl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.io.IOUtils;
import org.springframework.core.io.Resource;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.matconsolectl.pool.MatlabProxyObjectPool;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.concurrent.lock.disabled.DisabledLock;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.UniqueNameGenerator;
import matlabcontrol.MatlabInvocationException;
import matlabcontrol.MatlabProxy;
import matlabcontrol.extensions.MatlabTypeConverter;

@NotThreadSafe
public class MatConsoleCtlScriptTaskEngineMatlab implements IScriptTaskEngine {

    private static final UniqueNameGenerator ADD_PATH_GENERATOR = new UniqueNameGenerator();
    private MatlabProxy matlabProxy;
    private MatlabTypeConverter typeConverter;
    private final MatConsoleCtlScriptTaskInputsMatlab inputs;
    private final MatConsoleCtlScriptTaskResultsMatlab results;
    private String expressionEnding;
    private final File addPathDir;
    private boolean addPathEnabled;

    public MatConsoleCtlScriptTaskEngineMatlab(final MatlabProxy matlabProxy) {
        setMatlabProxy(matlabProxy);
        this.inputs = new MatConsoleCtlScriptTaskInputsMatlab(this);
        this.results = new MatConsoleCtlScriptTaskResultsMatlab(this);
        if (IScriptTaskRunnerMatlab.LOG.isDebugEnabled()) {
            expressionEnding = ";";
        } else {
            expressionEnding = "";
        }
        this.addPathDir = new File(ContextProperties.TEMP_DIRECTORY,
                MatConsoleCtlScriptTaskEngineMatlab.class.getSimpleName() + "/" + ADD_PATH_GENERATOR.get("addPath"));
    }

    public void setMatlabProxy(final MatlabProxy matlabProxy) {
        this.matlabProxy = matlabProxy;
        if (matlabProxy != null) {
            this.typeConverter = new MatlabTypeConverter(matlabProxy);
        } else {
            this.typeConverter = null;
        }
    }

    @Override
    public void eval(final String expression) {
        try {
            matlabProxy.eval(expression + expressionEnding);
        } catch (final MatlabInvocationException e) {
            throw new RuntimeException(e);
        }
    }

    public void addPath(final Resource resource) {
        if (!addPathEnabled) {
            try {
                Files.forceMkdir(addPathDir);
            } catch (final IOException e) {
                throw new RuntimeException(e);
            }
            eval("addpath('" + addPathDir.getAbsolutePath() + "')");
            addPathEnabled = true;
        }
        try (InputStream in = resource.getInputStream()) {
            try (OutputStream out = new FileOutputStream(new File(addPathDir, resource.getFilename()))) {
                IOUtils.copy(in, out);
            }
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public MatConsoleCtlScriptTaskInputsMatlab getInputs() {
        return inputs;
    }

    @Override
    public MatConsoleCtlScriptTaskResultsMatlab getResults() {
        return results;
    }

    @Override
    public void close() {
        if (addPathEnabled) {
            eval("rmpath('" + addPathDir.getAbsolutePath() + "')");
            Files.deleteNative(addPathDir);
            addPathEnabled = false;
        }
        matlabProxy = null;
        typeConverter = null;
    }

    @Override
    public MatlabProxy unwrap() {
        return matlabProxy;
    }

    public MatlabTypeConverter getTypeConverter() {
        return typeConverter;
    }

    public int pos2ind(final int[] size, final int... pos) {
        if (size.length != pos.length) {
            throw new IllegalArgumentException("There must be an equal number of lengths [" + size.length + "] "
                    + "and indices [" + pos.length + "]");
        }

        int linearIndex = 0;

        int accumSize = 1;
        for (int i = 0; i < size.length; i++) {
            linearIndex += accumSize * pos[i];
            accumSize *= size[i];
        }

        return linearIndex;
    }

    /**
     * Each instance has its own engine, so no shared locking required.
     */
    @Override
    public ILock getSharedLock() {
        return DisabledLock.INSTANCE;
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static MatConsoleCtlScriptTaskEngineMatlab newInstance() {
        return new MatConsoleCtlScriptTaskEngineMatlab(MatlabProxyObjectPool.INSTANCE.borrowObject()) {
            @Override
            public void close() {
                final MatlabProxy unwrap = unwrap();
                if (unwrap != null) {
                    MatlabProxyObjectPool.INSTANCE.returnObject(unwrap);
                }
                super.close();
            }
        };
    }

}
