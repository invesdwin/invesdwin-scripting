<T> T callback(String methodName, Object... parameters) {
    if(!binding.containsKey("luaScriptTaskCallbackContext")) {
        if(binding.containsKey("luaScriptTaskCallbackContextUuid")) {
            binding.put("luaScriptTaskCallbackContext", de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext.getContext(luaScriptTaskCallbackContextUuid));
        } else {
            throw new RuntimeException("IScriptTaskCallback not available");
        }
    }
    de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext context = (de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext) binding.get("luaScriptTaskCallbackContext");
    de.invesdwin.scripting.callback.ObjectScriptTaskReturnValue returnValue = context.invoke(methodName, parameters);
    if(returnValue.isReturnExpression()) {
    	de.invesdwin.scripting.runtime.lua.pool.WrappedLuaScriptEngine engine = de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool.INSTANCE.borrowObject();
    	try {
        	return (T) engine.eval((String) returnValue.getReturnValue(), binding);
        } finally {
        	de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool.INSTANCE.returnObject(engine);
        }
    } else {
        return (T) returnValue.getReturnValue();
    }
}