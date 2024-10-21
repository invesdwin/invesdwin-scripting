package de.invesdwin.scripting.runtime.beanshell;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerBeanshell {

    Log LOG = new Log(IScriptTaskRunnerBeanshell.class);

    <T> T run(AScriptTaskBeanshell<T> scriptTask);

}
