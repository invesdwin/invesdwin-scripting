print("putUuid")
print(putUuid)

SimpleCallbackTest = java.import("de.invesdwin.scripting.runtime.lua.tests.callback.SimpleCallbackTest")
getSecretStaticImport = SimpleCallbackTest:getSecretStatic(putUuid)
print("getSecretStaticImport")
print(getSecretStaticImport)

getSecretStaticCallback = callback("getSecretStatic", putUuid)
print("getSecretStaticCallback")
print(getSecretStaticCallback)

getSecretCallback = callback("getSecret", putUuid)
print("getSecretCallback")
print(getSecretCallback)

getSecretExpressionCallback = callback("getSecretExpression", putUuid)
print("getSecretExpressionCallback")
print(getSecretExpressionCallback)

callback("voidMethod")

callManyParams = callback("callManyParams", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if(callManyParams ~= 55) then
	error("callManyParams unexpected result: " .. callManyParams)
end
callManyParamsExpression = callback("callManyParamsExpression", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if(callManyParamsExpression ~= 55) then
	error("callManyParamsExpression unexpected result: " .. callManyParamsExpression)
end
callManyParamsExpressionMultiline = callback("callManyParamsExpressionMultiline", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if(callManyParamsExpressionMultiline ~= 55) then
	error("callManyParamsExpressionMultiline unexpected result: " .. callManyParamsExpressionMultiline)
end

getManyParamsExpression = putManyParamsExpression
print("getManyParamsExpression")
print(getManyParamsExpression)
getManyParamsExpressionMultiline = putManyParamsExpressionMultiline
print("getManyParamsExpressionMultiline")
print(getManyParamsExpressionMultiline)
