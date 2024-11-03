package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.scripting.IScriptTaskResults;

public interface IScriptTaskResultsRust extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return getBoolean("'" + variable + "' in locals() or '" + variable + "' in globals()");
    }

    @Override
    default boolean isNull(final String variable) {
        return getBoolean(variable + " is None");
    }

}
