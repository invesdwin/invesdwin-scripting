println!("putUuid: {}", putUuid);

let getSecretStaticCallback = callback("getSecretStatic", &[param(putUuid)]).as_string().unwrap();
println!("getSecretStaticCallback: {}", getSecretStaticCallback);

let getSecretCallback = callback("getSecret", &[param(putUuid)]).as_string().unwrap();
println!("getSecretCallback: {}", getSecretCallback);

let getSecretExpressionCallback = callback("getSecretExpression", &[param(putUuid)]).as_string().unwrap();
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
]).as_float().unwrap();
println!("callManyParams: {}", callManyParams);
if (callManyParams != 55.0) {
    panic!("callManyParams unexpected result: {}", callManyParams);
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
]).as_float().unwrap();
println!("callManyParamsExpression: {}", callManyParamsExpression);
if (callManyParamsExpression != 55.0) {
    panic!("callManyParamsExpression unexpected result: {}", callManyParamsExpression);
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
]).as_float().unwrap();
println!("callManyParamsExpressionMultiline: {}", callManyParamsExpressionMultiline);
if (callManyParamsExpressionMultiline != 55.0) {
    panic!("callManyParamsExpressionMultiline unexpected result: {}", callManyParamsExpressionMultiline);
}

let getManyParamsExpression = putManyParamsExpression;
println!("getManyParamsExpression: {}", getManyParamsExpression);
let getManyParamsExpressionMultiline = putManyParamsExpressionMultiline;
println!("getManyParamsExpressionMultiline: {}", getManyParamsExpressionMultiline);
