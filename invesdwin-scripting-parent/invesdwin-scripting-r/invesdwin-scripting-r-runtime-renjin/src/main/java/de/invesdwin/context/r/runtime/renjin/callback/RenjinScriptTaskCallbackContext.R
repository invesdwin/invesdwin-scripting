import(de.invesdwin.context.r.runtime.renjin.callback.RenjinScriptTaskCallbackContext)
import(de.invesdwin.context.integration.script.callback.ObjectScriptTaskReturnValue)
callback <- function(methodName, parameters = list()) {
    if(!exists("renjinScriptTaskCallbackContext")) {
        if(exists("renjinScriptTaskCallbackContextUuid")) {
            .GlobalEnv$renjinScriptTaskCallbackContext <- RenjinScriptTaskCallbackContext$getContext(renjinScriptTaskCallbackContextUuid)
        } else {
            stop("IScriptTaskCallback not available")
        }
    }
    returnValue = renjinScriptTaskCallbackContext$invoke(methodName, parameters)
    if(returnValue$isReturnExpression()){
		return(eval(parse(text=returnValue$getReturnValue())))
    } else {
    	return(returnValue$getReturnValue())
    }
}