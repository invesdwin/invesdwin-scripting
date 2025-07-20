package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.scripting.IScriptTaskResults;

public interface IScriptTaskResultsRust extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        try {
            getBoolean("f64::is_nan(" + variable + ")");
            return true;
        } catch (final Throwable t) {
            return !t.getMessage().startsWith("[E0425]");
        }
    }

    @Override
    default boolean isNull(final String variable) {
        try {
            return getBoolean("f64::is_nan(" + variable + ")");
        } catch (final Throwable t) {
            return false;
        }
    }

}
