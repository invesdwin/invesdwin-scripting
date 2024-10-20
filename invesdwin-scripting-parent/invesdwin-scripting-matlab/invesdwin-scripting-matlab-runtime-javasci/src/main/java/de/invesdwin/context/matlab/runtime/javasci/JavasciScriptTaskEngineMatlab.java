package de.invesdwin.context.matlab.runtime.javasci;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab;
import de.invesdwin.context.matlab.runtime.javasci.internal.ScilabWrapper;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.lock.ILock;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.UniqueNameGenerator;

@NotThreadSafe
public class JavasciScriptTaskEngineMatlab implements IScriptTaskEngine {

    private static final UniqueNameGenerator UNIQUE_NAME_GENERATOR = new UniqueNameGenerator() {
        @Override
        protected long getInitialValue() {
            return 1;
        }
    };
    private static final File FOLDER = new File(ContextProperties.TEMP_DIRECTORY,
            JavasciScriptTaskEngineMatlab.class.getSimpleName());

    static {
        Assertions.checkNotNull(JavasciProperties.JAVASCI_LIBRARY_PATHS);
    }

    private ScilabWrapper scilab;
    private final JavasciScriptTaskInputsMatlab inputs;
    private final JavasciScriptTaskResultsMatlab results;
    private String expressionEnding;
    private File scriptFile;

    public JavasciScriptTaskEngineMatlab(final ScilabWrapper scilab) {
        this.scilab = scilab;
        this.inputs = new JavasciScriptTaskInputsMatlab(this);
        this.results = new JavasciScriptTaskResultsMatlab(this);
        if (IScriptTaskRunnerMatlab.LOG.isDebugEnabled()) {
            expressionEnding = ";";
        } else {
            expressionEnding = "";
        }
        try {
            Files.forceMkdir(FOLDER);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        this.scriptFile = new File(FOLDER, UNIQUE_NAME_GENERATOR.get("script") + ".sce");
    }

    @Override
    public void eval(final String expression) {
        try {
            Files.writeStringToFile(scriptFile, expression + expressionEnding, Charset.defaultCharset());
            scilab.getScilab().execException(scriptFile);
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JavasciScriptTaskInputsMatlab getInputs() {
        return inputs;
    }

    @Override
    public JavasciScriptTaskResultsMatlab getResults() {
        return results;
    }

    @Override
    public void close() {
        eval(JavasciScriptTaskRunnerMatlab.CLEANUP_SCRIPT);
        Files.deleteQuietly(scriptFile);
        scriptFile = null;
        scilab = null;
    }

    @Override
    public ScilabWrapper unwrap() {
        return scilab;
    }

    /**
     * Always acquire this lock before accessing scilab. There is only one instance per JVM.
     */
    @Override
    public ILock getSharedLock() {
        return unwrap().getLock();
    }

    @Override
    public WrappedExecutorService getSharedExecutor() {
        return null;
    }

    public static JavasciScriptTaskEngineMatlab newInstance() {
        return new JavasciScriptTaskEngineMatlab(ScilabWrapper.INSTANCE);
    }

}
