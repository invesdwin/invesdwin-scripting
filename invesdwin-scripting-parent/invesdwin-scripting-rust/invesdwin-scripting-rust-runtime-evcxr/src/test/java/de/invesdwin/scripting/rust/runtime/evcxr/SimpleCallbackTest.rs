println!("putUuid: {}", putUuid);

let getSecretStaticCallback: String = callback("getSecretStatic", &[param(putUuid)]);
println!("getSecretStaticCallback: {}", getSecretStaticCallback);

let getSecretCallback: String = callback("getSecret", &[param(putUuid)]);
println!("getSecretCallback: {}", getSecretCallback);

let getSecretExpressionCallback: String = callback("getSecretExpression", &[param(putUuid)]);
println!("getSecretExpressionCallback: {}", getSecretExpressionCallback);

// callback_void or callback_dynamic would be slightly more efficient for void methods, but callback with unit type also works fine
callback::<()>("voidMethod", &[]);

let callManyParams: f64 = callback("callManyParams", &[
    param(true),
    param(2),
    param(3),
    param("4"),
    param(5),
    param(6),
    param(7.0),
    param(8.0),
    param("123456789"),
    param(10.0)
]);
println!("callManyParams: {}", callManyParams);
if (callManyParams != 55.0) {
    panic!("callManyParams unexpected result: {}", callManyParams);
}

let callManyParamsExpression: f64 = callback("callManyParamsExpression", &[
    param(true),
    param(2),
    param(3),
    param("4"),
    param(5),
    param(6),
    param(7.0),
    param(8.0),
    param("123456789"),
    param(10.0)
]);
println!("callManyParamsExpression: {}", callManyParamsExpression);
if (callManyParamsExpression != 55.0) {
    panic!("callManyParamsExpression unexpected result: {}", callManyParamsExpression);
}

let callManyParamsExpressionMultiline: f64 = callback("callManyParamsExpressionMultiline", &[
    param(true),
    param(2),
    param(3),
    param("4"),
    param(5),
    param(6),
    param(7.0),
    param(8.0),
    param("123456789"),
    param(10.0)
]);
println!("callManyParamsExpressionMultiline: {}", callManyParamsExpressionMultiline);
if (callManyParamsExpressionMultiline != 55.0) {
    panic!("callManyParamsExpressionMultiline unexpected result: {}", callManyParamsExpressionMultiline);
}

let getManyParamsExpression = putManyParamsExpression;
println!("getManyParamsExpression: {}", getManyParamsExpression);
let getManyParamsExpressionMultiline = putManyParamsExpressionMultiline;
println!("getManyParamsExpressionMultiline: {}", getManyParamsExpressionMultiline);
