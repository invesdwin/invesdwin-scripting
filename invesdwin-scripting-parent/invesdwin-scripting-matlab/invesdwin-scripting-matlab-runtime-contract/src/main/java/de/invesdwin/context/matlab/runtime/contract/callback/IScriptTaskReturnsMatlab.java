package de.invesdwin.context.matlab.runtime.contract.callback;

import de.invesdwin.context.integration.script.callback.IScriptTaskReturns;

public interface IScriptTaskReturnsMatlab extends IScriptTaskReturns {

    default void returnEmpty() {
        returnExpression("double([])");
    }

}
