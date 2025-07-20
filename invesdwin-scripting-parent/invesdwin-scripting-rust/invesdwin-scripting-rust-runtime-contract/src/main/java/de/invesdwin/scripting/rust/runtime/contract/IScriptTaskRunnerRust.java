package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerRust {

    Log LOG = new Log(IScriptTaskRunnerRust.class);

    <T> T run(AScriptTaskRust<T> scriptTask);

}
