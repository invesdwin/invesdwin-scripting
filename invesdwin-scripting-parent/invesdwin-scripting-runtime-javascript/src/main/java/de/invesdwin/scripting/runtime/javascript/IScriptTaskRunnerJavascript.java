package de.invesdwin.scripting.runtime.javascript;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerJavascript {

    Log LOG = new Log(IScriptTaskRunnerJavascript.class);

    <T> T run(AScriptTaskJavascript<T> scriptTask);

}
