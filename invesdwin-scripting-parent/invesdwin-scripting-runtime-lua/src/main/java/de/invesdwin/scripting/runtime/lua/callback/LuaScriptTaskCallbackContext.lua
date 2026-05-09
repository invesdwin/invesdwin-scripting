callback__LuaScriptTaskCallbackContext = java.import("de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext")
callback__LuaScriptEngineObjectPool = java.import("de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool")
callback_JavaObject = java.import("java.lang.Object")

function callback(methodName, ...)
	-- Debug: Log method name and arguments
	print("DEBUG: callback called with method:", methodName)
	print("DEBUG: arguments count:", #({...}))
	for i, v in ipairs({...}) do
	    print("DEBUG: arg[" .. i .. "] =", v, "(type:", type(v), ")")
	end
	
    if _G.luaScriptTaskCallbackContext == nil then
        if _G.luaScriptTaskCallbackContextUuid ~= nil then
            _G.luaScriptTaskCallbackContext = callback__LuaScriptTaskCallbackContext:getContext(_G.luaScriptTaskCallbackContextUuid)
        else
            error("IScriptTaskCallback not available")
        end
    end
    local context = _G.luaScriptTaskCallbackContext
    
    -- Wrap varargs into Java array for LuaJava compatibility
	local args = {...}
    -- Create the Java Object[] array
    local argsArray = java.array(callback_JavaObject, #args)
    for i, v in ipairs(args) do
        argsArray[i] = v
    end
	print("DEBUG: Invoking method on context with methodName:", methodName)
    local returnValue = context:invoke(methodName, argsArray)
    if returnValue:isReturnExpression() then
        local pool = callback__LuaScriptEngineObjectPool.INSTANCE
        local engine = pool:borrowObject()
        local success, result = pcall(function()
			print("DEBUG: Evaluating return expression:", returnValue:getReturnValue())
            return engine:getSingleJava(returnValue:getReturnValue())
        end)
        pool:returnObject(engine)
        if not success then
			print("DEBUG: Error evaluating return expression:", result)
            error(result)
        end
		print("DEBUG: Return expression evaluated successfully, result:", result)
        return result
    else
        return returnValue:getReturnValue()
    end
end