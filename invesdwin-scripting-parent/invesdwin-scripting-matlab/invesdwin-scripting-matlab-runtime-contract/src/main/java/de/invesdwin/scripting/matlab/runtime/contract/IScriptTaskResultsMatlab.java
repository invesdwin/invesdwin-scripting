package de.invesdwin.scripting.matlab.runtime.contract;

import de.invesdwin.scripting.IScriptTaskResults;

public interface IScriptTaskResultsMatlab extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return getBoolean("exist('" + variable + "')");
    }

}
