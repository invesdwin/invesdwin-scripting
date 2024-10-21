package de.invesdwin.scripting.haskell.runtime.contract;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsHaskell extends IScriptTaskInputs {

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval(variable + " = " + expression);
    }

    /**
     * frege/ghci does not really support Nothing/null, causes exceptions when "show" or "encode" is called on it, so we
     * use empty string as a workaround
     */
    @Override
    default void putNull(final String variable) {
        putExpression(variable, "\"\"");
    }

    @Override
    default void remove(final String variable) {
        putNull(variable);
    }

}
