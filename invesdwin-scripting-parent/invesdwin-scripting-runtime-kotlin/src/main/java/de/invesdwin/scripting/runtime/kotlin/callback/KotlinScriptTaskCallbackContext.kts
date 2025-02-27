fun <T> callback(methodName: String, vararg parameters: Any?): T {
    if(!binding.containsKey("kotlinScriptTaskCallbackContext")) {
        if(binding.containsKey("kotlinScriptTaskCallbackContextUuid")) {
            binding.put("kotlinScriptTaskCallbackContext", de.invesdwin.scripting.runtime.kotlin.callback.KotlinScriptTaskCallbackContext.getContext(kotlinScriptTaskCallbackContextUuid));
        } else {
            throw Exception("IScriptTaskCallback not available");
        }
    }
    val context: de.invesdwin.scripting.runtime.kotlin.callback.KotlinScriptTaskCallbackContext = binding.get("kotlinScriptTaskCallbackContext") as de.invesdwin.scripting.runtime.kotlin.callback.KotlinScriptTaskCallbackContext;
    val returnValue: de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue = context.invoke(methodName, *parameters);
    if(returnValue.isReturnExpression()) {
    	val engine: de.invesdwin.scripting.runtime.kotlin.pool.WrappedKotlinScriptEngine = de.invesdwin.scripting.runtime.kotlin.pool.KotlinScriptEngineObjectPool.INSTANCE.borrowObject();
    	try {
    		//can not pass bindings here, else classcastexceptions occur
        	return engine.eval(returnValue.getReturnValue() as String) as T;
        } finally {
        	de.invesdwin.scripting.runtime.kotlin.pool.KotlinScriptEngineObjectPool.INSTANCE.returnObject(engine);
        }
    } else {
        return returnValue.getReturnValue() as T;
    }
}