package de.invesdwin.scripting.runtime.kotlin;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerKotlin {

    Log LOG = new Log(IScriptTaskRunnerKotlin.class);

    <T> T run(AScriptTaskKotlin<T> scriptTask);

}
