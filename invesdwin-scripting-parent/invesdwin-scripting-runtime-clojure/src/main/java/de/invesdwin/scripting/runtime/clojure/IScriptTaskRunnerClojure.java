package de.invesdwin.scripting.runtime.clojure;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerClojure {

    Log LOG = new Log(IScriptTaskRunnerClojure.class);

    <T> T run(AScriptTaskClojure<T> scriptTask);

}
