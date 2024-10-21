package de.invesdwin.scripting.haskell.runtime.contract;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerHaskell {

    Log LOG = new Log(IScriptTaskRunnerHaskell.class);

    <T> T run(AScriptTaskHaskell<T> scriptTask);

}
