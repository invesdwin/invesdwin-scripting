package de.invesdwin.scripting.r.runtime.contract;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerR {

    String CLEANUP_SCRIPT = "rm(list=ls(all=TRUE))";

    Log LOG = new Log(IScriptTaskRunnerR.class);

    <T> T run(AScriptTaskR<T> scriptTask);

}
