package de.invesdwin.scripting.julia.runtime.contract;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerJulia {

    Log LOG = new Log(IScriptTaskRunnerJulia.class);

    <T> T run(AScriptTaskJulia<T> scriptTask);

}
