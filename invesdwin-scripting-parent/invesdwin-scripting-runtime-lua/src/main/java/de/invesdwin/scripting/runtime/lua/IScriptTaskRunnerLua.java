package de.invesdwin.scripting.runtime.lua;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerLua {

    Log LOG = new Log(IScriptTaskRunnerLua.class);

    <T> T run(AScriptTaskLua<T> scriptTask);

}
