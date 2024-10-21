package de.invesdwin.scripting.julia.runtime.contract;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsJulia extends IScriptTaskInputs {

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval(variable + " = " + expression);
    }

    @Override
    default void putNull(final String variable) {
        putExpression(variable, "nothing");
    }

    @Override
    default void remove(final String variable) {
        putNull(variable);
    }

}
