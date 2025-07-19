package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsRust extends IScriptTaskInputs {

    String NAN = "f64::NAN";

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval("let " + variable + " = " + expression + ";");
    }

    @Override
    default void putNull(final String variable) {
        putExpression(variable, NAN);
    }

    @Override
    default void remove(final String variable) {
        putNull(variable);
    }

}
