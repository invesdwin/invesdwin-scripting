package de.invesdwin.context.julia.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskInputs;

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
