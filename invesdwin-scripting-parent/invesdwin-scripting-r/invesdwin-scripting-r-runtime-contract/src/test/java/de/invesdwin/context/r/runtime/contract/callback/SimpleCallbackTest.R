print("putUuid")
print(putUuid)

getSecretStaticCallback <- callback("getSecretStatic", putUuid)
print("getSecretStaticCallback")
print(getSecretStaticCallback)

getSecretCallback <- callback("getSecret", putUuid)
print("getSecretCallback")
print(getSecretCallback)

getSecretExpressionCallback <- callback("getSecretExpression", putUuid)
print("getSecretExpressionCallback")
print(getSecretExpressionCallback)

callback("voidMethod")

callManyParams <- callback("callManyParams", list(TRUE, 2, 3, "4", 5, 6, 7.0, 8.0, "123456789", 10.0))
if(callManyParams != 55){
	stop(paste("callManyParams unexpected result: ",callManyParams))
}
callManyParamsExpression <- callback("callManyParamsExpression", list(TRUE, 2, 3, "4", 5, 6, 7.0, 8.0, "123456789", 10.0))
if(callManyParamsExpression != 55){
	stop(paste("callManyParamsExpression unexpected result: ",callManyParamsExpression))
}
callManyParamsExpressionMultiline <- callback("callManyParamsExpressionMultiline", list(TRUE, 2, 3, "4", 5, 6, 7.0, 8.0, "123456789", 10.0))
if(callManyParamsExpressionMultiline != 55){
	stop(paste("callManyParamsExpressionMultiline unexpected result: ",callManyParamsExpressionMultiline))
}

getManyParamsExpression <- putManyParamsExpression
print("getManyParamsExpression")
print(getManyParamsExpression)
getManyParamsExpressionMultilineWrong <- putManyParamsExpressionMultilineWrong
print("getManyParamsExpressionMultilineWrong")
print(getManyParamsExpressionMultilineWrong)
getManyParamsExpressionMultiline <- putManyParamsExpressionMultiline
print("getManyParamsExpressionMultiline")
print(getManyParamsExpressionMultiline)
