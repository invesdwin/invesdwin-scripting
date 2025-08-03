package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.matlab.runtime.contract.AScriptTaskInputsMatlabToExpression;
import de.invesdwin.scripting.matlab.runtime.jascib.callback.file.JavasciScriptTaskReturnsMatlabToExpression;

@NotThreadSafe
public class JascibScriptTaskInputsMatlab extends AScriptTaskInputsMatlabToExpression {

    private final JascibScriptTaskEngineMatlab engine;

    public JascibScriptTaskInputsMatlab(final JascibScriptTaskEngineMatlab engine) {
        this.engine = engine;
    }

    @Override
    public JascibScriptTaskEngineMatlab getEngine() {
        return engine;
    }

    @Override
    protected String getFloatType() {
        return "double";
    }

    @Override
    protected String booleanToString(final boolean value) {
        return JavasciScriptTaskReturnsMatlabToExpression.booleanToString(value);
    }

}
