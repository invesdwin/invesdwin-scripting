package de.invesdwin.scripting.ruby.runtime;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerRuby {

    Log LOG = new Log(IScriptTaskRunnerRuby.class);

    <T> T run(AScriptTaskRuby<T> scriptTask);

}
