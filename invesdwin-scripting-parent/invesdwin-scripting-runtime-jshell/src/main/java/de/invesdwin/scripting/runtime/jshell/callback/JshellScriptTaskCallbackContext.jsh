<T> T callback(String methodName, Object... parameters) {
    if(!binding.containsKey("jshellScriptTaskCallbackContext")) {
        if(binding.containsKey("jshellScriptTaskCallbackContextUuid")) {
            binding.put("jshellScriptTaskCallbackContext", de.invesdwin.scripting.runtime.jshell.callback.JshellScriptTaskCallbackContext.getContext(jshellScriptTaskCallbackContextUuid));
        } else {
            throw new RuntimeException("IScriptTaskCallback not available");
        }
    }
    de.invesdwin.scripting.runtime.jshell.callback.JshellScriptTaskCallbackContext context = (de.invesdwin.scripting.runtime.jshell.callback.JshellScriptTaskCallbackContext) binding.get("jshellScriptTaskCallbackContext");
    de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue returnValue = context.invoke(methodName, parameters);
    if(returnValue.isReturnExpression()) {
    	de.invesdwin.scripting.runtime.jshell.pool.WrappedJshellScriptEngine engine = de.invesdwin.scripting.runtime.jshell.pool.JshellScriptEngineObjectPool.INSTANCE.borrowObject();
    	try {
        	return (T) engine.eval((String) returnValue.getReturnValue(), binding);
        } finally {
        	de.invesdwin.scripting.runtime.jshell.pool.JshellScriptEngineObjectPool.INSTANCE.returnObject(engine);
        }
    } else {
        return (T) returnValue.getReturnValue();
    }
}