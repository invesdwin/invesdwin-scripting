callback__LuaScriptTaskCallbackContext = java.import("de.invesdwin.scripting.runtime.lua.callback.LuaScriptTaskCallbackContext")
callback__LuaScriptEngineObjectPool = java.import("de.invesdwin.scripting.runtime.lua.pool.LuaScriptEngineObjectPool")

function callback(methodName, ...)
    if _G.luaScriptTaskCallbackContext == nil then
        if _G.luaScriptTaskCallbackContextUuid ~= nil then
            _G.luaScriptTaskCallbackContext = callback__LuaScriptTaskCallbackContext:getContext(_G.luaScriptTaskCallbackContextUuid)
        else
            error("IScriptTaskCallback not available")
        end
    end
    local context = _G.luaScriptTaskCallbackContext
    local returnValue = context:invoke(methodName, ...)
    if returnValue:isReturnExpression() then
        local pool = callback__LuaScriptEngineObjectPool.INSTANCE
        local engine = pool:borrowObject()
        local success, result = pcall(function()
            return engine:eval(returnValue:getReturnValue())
        end)
        pool:returnObject(engine)
        if not success then
            error(result)
        end
        return result
    else
        return returnValue:getReturnValue()
    end
end