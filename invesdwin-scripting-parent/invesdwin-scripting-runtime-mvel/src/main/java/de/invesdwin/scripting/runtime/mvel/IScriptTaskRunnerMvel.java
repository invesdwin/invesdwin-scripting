package de.invesdwin.scripting.runtime.mvel;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerMvel {

    Log LOG = new Log(IScriptTaskRunnerMvel.class);

    <T> T run(AScriptTaskMvel<T> scriptTask);

}
