package de.invesdwin.scripting.matlab.runtime.contract;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsMatlab extends IScriptTaskInputs {

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval(variable + " = " + expression);
    }

    default void putEmpty(final String variable) {
        putExpression(variable, "double([])");
    }

    @Override
    default void remove(final String variable) {
        getEngine().eval("clear('" + variable + "')");
    }

}
