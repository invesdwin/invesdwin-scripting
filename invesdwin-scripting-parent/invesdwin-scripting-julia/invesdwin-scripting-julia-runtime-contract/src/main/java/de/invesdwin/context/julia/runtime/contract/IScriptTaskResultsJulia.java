package de.invesdwin.context.julia.runtime.contract;

import de.invesdwin.context.integration.script.IScriptTaskResults;

public interface IScriptTaskResultsJulia extends IScriptTaskResults {

    @Override
    default boolean isDefined(final String variable) {
        return getBoolean("isdefined(Main, :" + variable + ")");
    }

    default boolean isDefinedNotNull(final String variable) {
        return getBoolean("isdefined(Main, :" + variable + ") && !isnothing(" + variable + ")");
    }

    default boolean isNotDefinedOrNull(final String variable) {
        return getBoolean("!isdefined(Main, :" + variable + ") || isnothing(" + variable + ")");
    }

    @Override
    default boolean isNull(final String variable) {
        return getBoolean("isnothing(" + variable + ")");
    }

    default boolean isEmpty(final String variable) {
        return getBoolean("isempty(" + variable + ")");
    }

}
