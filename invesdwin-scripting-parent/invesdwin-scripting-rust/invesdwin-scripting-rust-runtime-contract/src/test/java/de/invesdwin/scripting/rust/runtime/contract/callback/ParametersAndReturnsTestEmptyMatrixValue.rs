let getBooleanMatrix: Vec<Vec<bool>> = callback("getBooleanMatrix", &[]);
if getBooleanMatrix.len() != 2 {
    panic!("getBooleanMatrix empty!");
}
callback_void("setBooleanMatrix", &[param(getBooleanMatrix)]);

let getBooleanMatrixAsList: Vec<Vec<bool>> = callback("getBooleanMatrixAsList", &[]);
if getBooleanMatrixAsList.len() != 2 {
    panic!("getBooleanMatrixAsList empty!");
}
callback_void("setBooleanMatrixAsList", &[param(getBooleanMatrixAsList)]);

let getByteMatrix: Vec<Vec<i32>> = callback("getByteMatrix", &[]);
if getByteMatrix.len() != 2 {
    panic!("getByteMatrix empty!");
}
callback_void("setByteMatrix", &[param(getByteMatrix)]);

let getByteMatrixAsList: Vec<Vec<i32>> = callback("getByteMatrixAsList", &[]);
if getByteMatrixAsList.len() != 2 {
    panic!("getByteMatrixAsList empty!");
}
callback_void("setByteMatrixAsList", &[param(getByteMatrixAsList)]);

let getCharacterMatrix: Vec<Vec<String>> = callback("getCharacterMatrix", &[]);
if getCharacterMatrix.len() != 2 {
    panic!("getCharacterMatrix empty!");
}
callback_void("setCharacterMatrix", &[param(getCharacterMatrix)]);

let getCharacterMatrixAsList: Vec<Vec<String>> = callback("getCharacterMatrixAsList", &[]);
if getCharacterMatrixAsList.len() != 2 {
    panic!("getCharacterMatrixAsList empty!");
}
callback_void("setCharacterMatrixAsList", &[param(getCharacterMatrixAsList)]);

let getDecimalMatrix: Vec<Vec<f64>> = callback("getDecimalMatrix", &[]);
if getDecimalMatrix.len() != 2 {
    panic!("getDecimalMatrix empty!");
}
callback_void("setDecimalMatrix", &[param(getDecimalMatrix)]);

let getDecimalMatrixAsList: Vec<Vec<f64>> = callback("getDecimalMatrixAsList", &[]);
if getDecimalMatrixAsList.len() != 2 {
    panic!("getDecimalMatrixAsList empty!");
}
callback_void("setDecimalMatrixAsList", &[param(getDecimalMatrixAsList)]);

let getDoubleMatrix: Vec<Vec<f64>> = callback("getDoubleMatrix", &[]);
if getDoubleMatrix.len() != 2 {
    panic!("getDoubleMatrix empty!");
}
callback_void("setDoubleMatrix", &[param(getDoubleMatrix)]);

let getDoubleMatrixAsList: Vec<Vec<f64>> = callback("getDoubleMatrixAsList", &[]);
if getDoubleMatrixAsList.len() != 2 {
    panic!("getDoubleMatrixAsList empty!");
}
callback_void("setDoubleMatrixAsList", &[param(getDoubleMatrixAsList)]);

let getFloatMatrix: Vec<Vec<f64>> = callback("getFloatMatrix", &[]);
if getFloatMatrix.len() != 2 {
    panic!("getFloatMatrix empty!");
}
callback_void("setFloatMatrix", &[param(getFloatMatrix)]);

let getFloatMatrixAsList: Vec<Vec<f64>> = callback("getFloatMatrixAsList", &[]);
if getFloatMatrixAsList.len() != 2 {
    panic!("getFloatMatrixAsList empty!");
}
callback_void("setFloatMatrixAsList", &[param(getFloatMatrixAsList)]);

let getIntegerMatrix: Vec<Vec<i32>> = callback("getIntegerMatrix", &[]);
if getIntegerMatrix.len() != 2 {
    panic!("getIntegerMatrix empty!");
}
callback_void("setIntegerMatrix", &[param(getIntegerMatrix)]);

let getIntegerMatrixAsList: Vec<Vec<i32>> = callback("getIntegerMatrixAsList", &[]);
if getIntegerMatrixAsList.len() != 2 {
    panic!("getIntegerMatrixAsList empty!");
}
callback_void("setIntegerMatrixAsList", &[param(getIntegerMatrixAsList)]);

let getLongMatrix: Vec<Vec<i64>> = callback("getLongMatrix", &[]);
if getLongMatrix.len() != 2 {
    panic!("getLongMatrix empty!");
}
callback_void("setLongMatrix", &[param(getLongMatrix)]);

let getLongMatrixAsList: Vec<Vec<i64>> = callback("getLongMatrixAsList", &[]);
if getLongMatrixAsList.len() != 2 {
    panic!("getLongMatrixAsList empty!");
}
callback_void("setLongMatrixAsList", &[param(getLongMatrixAsList)]);

let getPercentMatrix: Vec<Vec<f64>> = callback("getPercentMatrix", &[]);
if getPercentMatrix.len() != 2 {
    panic!("getPercentMatrix empty!");
}
callback_void("setPercentMatrix", &[param(getPercentMatrix)]);

let getPercentMatrixAsList: Vec<Vec<f64>> = callback("getPercentMatrixAsList", &[]);
if getPercentMatrixAsList.len() != 2 {
    panic!("getPercentMatrixAsList empty!");
}
callback_void("setPercentMatrixAsList", &[param(getPercentMatrixAsList)]);

let getShortMatrix: Vec<Vec<i16>> = callback("getShortMatrix", &[]);
if getShortMatrix.len() != 2 {
    panic!("getShortMatrix empty!");
}
callback_void("setShortMatrix", &[param(getShortMatrix)]);

let getShortMatrixAsList: Vec<Vec<i16>> = callback("getShortMatrixAsList", &[]);
if getShortMatrixAsList.len() != 2 {
    panic!("getShortMatrixAsList empty!");
}
callback_void("setShortMatrixAsList", &[param(getShortMatrixAsList)]);

let getStringMatrix: Vec<Vec<String>> = callback("getStringMatrix", &[]);
if getStringMatrix.len() != 2 {
    panic!("getStringMatrix empty!");
}
callback_void("setStringMatrix", &[param(getStringMatrix)]);

let getStringMatrixAsList: Vec<Vec<String>> = callback("getStringMatrixAsList", &[]);
if getStringMatrixAsList.len() != 2 {
    panic!("getStringMatrixAsList empty!");
}
callback_void("setStringMatrixAsList", &[param(getStringMatrixAsList)]);
