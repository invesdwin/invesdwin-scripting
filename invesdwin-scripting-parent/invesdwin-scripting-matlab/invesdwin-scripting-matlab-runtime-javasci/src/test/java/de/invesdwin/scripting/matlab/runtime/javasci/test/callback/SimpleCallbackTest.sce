disp('putUuid')
disp(putUuid)

getSecretStaticCallback = callback('getSecretStatic', putUuid)
disp('getSecretStaticCallback')
disp(getSecretStaticCallback)

getSecretCallback = callback('getSecret', putUuid)
disp('getSecretCallback')
disp(getSecretCallback)

getSecretExpressionCallback = callback('getSecretExpression', putUuid)
disp('getSecretExpressionCallback')
disp(getSecretExpressionCallback)

callback('voidMethod')

callManyParams = callback('callManyParams', %T, 2, 3, '4', 5, 6, 7.0, 8.0, '123456789', 10.0)
if callManyParams ~= 55
	error(strcat('callManyParams unexpected result: ',callManyParams))
end
callManyParamsExpression = callback('callManyParamsExpression', %T, 2, 3, '4', 5, 6, 7.0, 8.0, '123456789', 10.0)
if callManyParamsExpression ~= 55
	error(strcat('callManyParamsExpression unexpected result: ',callManyParamsExpression))
end
callManyParamsExpressionMultiline = callback('callManyParamsExpressionMultiline', %T, 2, 3, '4', 5, 6, 7.0, 8.0, '123456789', 10.0)
if callManyParamsExpressionMultiline ~= 55
	error(strcat('callManyParamsExpressionMultiline unexpected result: ',callManyParamsExpressionMultiline))
end

getManyParamsExpression = putManyParamsExpression
disp('getManyParamsExpression')
disp(getManyParamsExpression)
getManyParamsExpressionMultilineWrong = putManyParamsExpressionMultilineWrong
disp('getManyParamsExpressionMultilineWrong')
disp(getManyParamsExpressionMultilineWrong)
getManyParamsExpressionMultiline = putManyParamsExpressionMultiline
disp('getManyParamsExpressionMultiline')
disp(getManyParamsExpressionMultiline)
