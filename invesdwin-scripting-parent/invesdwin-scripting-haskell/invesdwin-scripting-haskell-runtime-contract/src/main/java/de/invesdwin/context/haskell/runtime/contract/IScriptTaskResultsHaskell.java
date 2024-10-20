package de.invesdwin.context.haskell.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskResults;

public interface IScriptTaskResultsHaskell extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return !isNull(variable);
    }

    default boolean isDefinedNotNull(final String variable) {
        return !isNull(variable);
    }

    default boolean isNotDefinedOrNull(final String variable) {
        return isNull(variable);
    }

    @Override
    default boolean isNull(final String variable) {
        return getBoolean(variable + " == \"\"");
    }

    default boolean isEmpty(final String variable) {
        return getBoolean("length (" + variable + ") == 0");
    }

    default int length(final String variable) {
        return getInteger("length (" + variable + ")");
    }

}
