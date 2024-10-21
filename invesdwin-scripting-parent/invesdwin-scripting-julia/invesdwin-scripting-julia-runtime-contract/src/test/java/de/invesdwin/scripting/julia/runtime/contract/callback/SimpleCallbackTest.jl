println("putUuid")
println(putUuid)

getSecretStaticCallback = callback("getSecretStatic", putUuid)
println("getSecretStaticCallback")
println(getSecretStaticCallback)

getSecretCallback = callback("getSecret", putUuid)
println("getSecretCallback")
println(getSecretCallback)

getSecretExpressionCallback = callback("getSecretExpression", putUuid)
println("getSecretExpressionCallback")
println(getSecretExpressionCallback)

callback("voidMethod")

callManyParams = callback("callManyParams", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if callManyParams != 55
	error("callManyParams unexpected result: $(callManyParams)")
end
callManyParamsExpression = callback("callManyParamsExpression", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if callManyParamsExpression != 55
	error("callManyParamsExpression unexpected result: $(callManyParamsExpression)")
end
callManyParamsExpressionMultiline = callback("callManyParamsExpressionMultiline", true, 2, 3, '4', 5, 6, 7.0, 8.0, "123456789", 10.0)
if callManyParamsExpressionMultiline != 55
	error("callManyParamsExpressionMultiline unexpected result: $(callManyParamsExpressionMultiline)")
end

getManyParamsExpression = putManyParamsExpression
println("getManyParamsExpression")
println(getManyParamsExpression)
getManyParamsExpressionMultilineWrong = putManyParamsExpressionMultilineWrong
println("getManyParamsExpressionMultilineWrong")
println(getManyParamsExpressionMultilineWrong)
getManyParamsExpressionMultiline = putManyParamsExpressionMultiline
println("getManyParamsExpressionMultiline")
println(getManyParamsExpressionMultiline)
