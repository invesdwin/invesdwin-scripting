println!("putUuid: {}", putUuid);

// Test callback with string return
let getSecretStaticCallback = callback("getSecretStatic", &putUuid)?;
println!("getSecretStaticCallback: {}", getSecretStaticCallback);

// Test callback with integer return
let getSecretCallback = callback("getSecret", &putUuid)?;
println!("getSecretCallback: {}", getSecretCallback.as_int().unwrap_or(-1));

// Test callback with expression evaluation
let getSecretExpressionCallback = callback("getSecretExpression", &putUuid)?;
println!("getSecretExpressionCallback: {}", getSecretExpressionCallback);

// Test void method (no return)
let voidResult = callback("voidMethod", &[])?;
println!("voidMethod: {:?}", voidResult);

// Test callback with many parameters and integer return
let callManyParams = callback("callManyParams", &true, &2, &3, &"4", &5, &6, &7.0, &"123456789", &10.0)?;
let callManyParamsInt = callManyParams.as_int().unwrap_or(-1);
println!("callManyParams: {}", callManyParamsInt);
if callManyParamsInt != 55 {
    panic!("callManyParams unexpected result: {}", callManyParamsInt);
}

// Test callback with many parameters and expression evaluation
let callManyParamsExpression = callback("callManyParamsExpression", &true, &2, &3, &"4", &5, &6, &7.0, &"123456789", &10.0)?;
let callManyParamsExpressionInt = callManyParamsExpression.as_int().unwrap_or(-1);
println!("callManyParamsExpression: {}", callManyParamsExpressionInt);
if callManyParamsExpressionInt != 55 {
    panic!("callManyParamsExpression unexpected result: {}", callManyParamsExpressionInt);
}

// Test callback with many parameters and multiline expression
let callManyParamsExpressionMultiline = callback("callManyParamsExpressionMultiline", &true, &2, &3, &"4", &5, &6, &7.0, &"123456789", &10.0)?;
let callManyParamsExpressionMultilineInt = callManyParamsExpressionMultiline.as_int().unwrap_or(-1);
println!("callManyParamsExpressionMultiline: {}", callManyParamsExpressionMultilineInt);
if callManyParamsExpressionMultilineInt != 55 {
    panic!("callManyParamsExpressionMultiline unexpected result: {}", callManyParamsExpressionMultilineInt);
}

// Test callback with wrong multiline expression (should handle gracefully)
let getManyParamsExpressionMultilineWrong = callback("getManyParamsExpressionMultilineWrong", &[])?;
println!("getManyParamsExpressionMultilineWrong: {}", getManyParamsExpressionMultilineWrong);

let getManyParamsExpressionMultiline = callback("getManyParamsExpressionMultiline", &[])?;
println!("getManyParamsExpressionMultiline: {}", getManyParamsExpressionMultiline);
