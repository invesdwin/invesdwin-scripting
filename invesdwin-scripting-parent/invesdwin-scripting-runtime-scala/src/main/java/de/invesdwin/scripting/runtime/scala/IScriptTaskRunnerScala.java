package de.invesdwin.scripting.runtime.scala;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerScala {

    Log LOG = new Log(IScriptTaskRunnerScala.class);

    <T> T run(AScriptTaskScala<T> scriptTask);

}
