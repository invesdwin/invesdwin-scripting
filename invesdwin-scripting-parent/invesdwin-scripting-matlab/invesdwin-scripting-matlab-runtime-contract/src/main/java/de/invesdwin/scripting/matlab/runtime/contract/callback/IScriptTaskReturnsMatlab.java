package de.invesdwin.scripting.matlab.runtime.contract.callback;

import de.invesdwin.scripting.callback.IScriptTaskReturns;

public interface IScriptTaskReturnsMatlab extends IScriptTaskReturns {

    default void returnEmpty() {
        returnExpression("double([])");
    }

}
