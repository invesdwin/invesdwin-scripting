package de.invesdwin.context.matlab.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskResults;

public interface IScriptTaskResultsMatlab extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return getBoolean("exist('" + variable + "')");
    }

}
