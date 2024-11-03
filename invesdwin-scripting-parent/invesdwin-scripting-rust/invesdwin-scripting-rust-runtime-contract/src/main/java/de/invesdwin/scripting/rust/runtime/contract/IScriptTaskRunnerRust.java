package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.context.log.Log;

public interface IScriptTaskRunnerRust {

    String CLEANUP_SCRIPT = "for var in list(globals()):\n" //
            + "    if var[0] == '_': continue\n" //
            + "    del globals()[var]\n";

    Log LOG = new Log(IScriptTaskRunnerRust.class);

    <T> T run(AScriptTaskRust<T> scriptTask);

}
