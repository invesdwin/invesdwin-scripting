println!("putUuid: {}", putUuid);

let getSecretStaticCallback = callback("getSecretStatic", &[param(putUuid)]);
println!("getSecretStaticCallback: {}", getSecretStaticCallback);

let getSecretCallback = callback("getSecret", &[param(putUuid)]);
println!("getSecretCallback: {}", getSecretCallback);

let getSecretExpressionCallback = callback("getSecretExpression", &[param(putUuid)]);
println!("getSecretExpressionCallback: {}", getSecretExpressionCallback);

callback("voidMethod", &[]);

let callManyParams = callback("callManyParams", &[
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
let callManyParamsFloat = callManyParams.as_float().unwrap_or(-1.0);
println!("callManyParams: {}", callManyParamsFloat);
if (callManyParamsFloat - 55.0).abs() > f64::EPSILON {
    panic!("callManyParams unexpected result: {}", callManyParamsFloat);
}

let callManyParamsExpression = callback("callManyParamsExpression", &[
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
let callManyParamsExpressionFloat = callManyParamsExpression.as_float().unwrap_or(-1.0);
println!("callManyParamsExpression: {}", callManyParamsExpressionFloat);
if (callManyParamsExpressionFloat - 55.0).abs() > f64::EPSILON {
    panic!("callManyParamsExpression unexpected result: {}", callManyParamsExpressionFloat);
}

let callManyParamsExpressionMultiline = callback("callManyParamsExpressionMultiline", &[
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
let callManyParamsExpressionMultilineFloat = callManyParamsExpressionMultiline.as_float().unwrap_or(-1.0);
println!("callManyParamsExpressionMultiline: {}", callManyParamsExpressionMultilineFloat);
if (callManyParamsExpressionMultilineFloat - 55.0).abs() > f64::EPSILON {
    panic!("callManyParamsExpressionMultiline unexpected result: {}", callManyParamsExpressionMultilineFloat);
}

let getManyParamsExpression = putManyParamsExpression;
println!("getManyParamsExpression: {}", getManyParamsExpression);
let getManyParamsExpressionMultiline = putManyParamsExpressionMultiline;
println!("getManyParamsExpressionMultiline: {}", getManyParamsExpressionMultiline);
