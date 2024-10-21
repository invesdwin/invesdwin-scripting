package de.invesdwin.scripting.ruby.runtime;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsRuby extends IScriptTaskInputs {

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval(variable + " = " + expression);
    }

}
