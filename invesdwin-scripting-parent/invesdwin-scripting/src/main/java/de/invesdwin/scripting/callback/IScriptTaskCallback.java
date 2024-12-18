package de.invesdwin.scripting.callback;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public interface IScriptTaskCallback {

    void invoke(String methodName, IScriptTaskParameters parameters, IScriptTaskReturns returns);

}
