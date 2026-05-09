LuaScriptTaskCallbackContext = java.import("de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext")
LuaScriptEngineObjectPool = java.import("de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool")

function callback(methodName, ...)
    if _G.luaScriptTaskCallbackContext == nil then
        if _G.luaScriptTaskCallbackContextUuid ~= nil then
            _G.luaScriptTaskCallbackContext = LuaScriptTaskCallbackContext:getContext(_G.luaScriptTaskCallbackContextUuid)
        else
            error("IScriptTaskCallback not available")
        end
    end

    local context = _G.luaScriptTaskCallbackContext
    -- Passing '...' (varargs) to Java works natively in this library
    local returnValue = context:invoke(methodName, ...)

    if returnValue:isReturnExpression() then
        -- Accessing static INSTANCE field and borrowing object
        local pool = LuaScriptEngineObjectPool.INSTANCE
        local engine = pool:borrowObject()
        
        -- Use pcall (protected call) to handle potential Java ScriptExceptions
        local success, result = pcall(function()
            return engine:eval(returnValue:getReturnValue())
        end)
        
        -- Ensure the object is returned to the pool even if eval fails
        pool:returnObject(engine)
        
        if not success then
            error(result)
        end
        return result
    else
        return returnValue:getReturnValue()
    end
end