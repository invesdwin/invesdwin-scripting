package de.invesdwin.scripting.runtime.groovy;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerGroovy {

    Log LOG = new Log(IScriptTaskRunnerGroovy.class);

    <T> T run(AScriptTaskGroovy<T> scriptTask);

}
