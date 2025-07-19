package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.scripting.IScriptTaskResults;

public interface IScriptTaskResultsRust extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return !isNull(variable);
    }

    @Override
    default boolean isNull(final String variable) {
        return getBoolean("f64::is_nan(" + variable + ")");
    }

}
