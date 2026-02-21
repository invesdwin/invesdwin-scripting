println!("getDouble");
let getDouble: f64 = callback("getDouble", &[]);
println!("getDouble: {}", getDouble);
if !getDouble.is_nan() {
    panic!("getDouble not NaN!");
}
callback_void("setDouble", &[param(getDouble)]);

println!("getDoubleVector");
let getDoubleVector: Vec<f64> = callback("getDoubleVector", &[]);
println!("getDoubleVector: {:?}", getDoubleVector);
if !getDoubleVector[1].is_nan() {
    panic!("getDoubleVector[1] not NaN!");
}
callback_void("setDoubleVector", &[param(getDoubleVector)]);

println!("getDoubleVectorAsList");
let getDoubleVectorAsList: Vec<f64> = callback("getDoubleVectorAsList", &[]);
println!("getDoubleVectorAsList: {:?}", getDoubleVectorAsList);
if !getDoubleVectorAsList[1].is_nan() {
    panic!("getDoubleVectorAsList[1] not NaN!");
}
callback_void("setDoubleVectorAsList", &[param(getDoubleVectorAsList)]);

println!("getDoubleMatrix");
let getDoubleMatrix: Vec<Vec<f64>> = callback("getDoubleMatrix", &[]);
println!("getDoubleMatrix: {:?}", getDoubleMatrix);
if !getDoubleMatrix[0][0].is_nan() {
    panic!("getDoubleMatrix[0][0] not NaN!");
}
if !getDoubleMatrix[1][1].is_nan() {
    panic!("getDoubleMatrix[1][1] not NaN!");
}
if !getDoubleMatrix[2][2].is_nan() {
    panic!("getDoubleMatrix[2][2] not NaN!");
}
callback_void("setDoubleMatrix", &[param(getDoubleMatrix)]);

println!("getDoubleMatrixAsList");
let getDoubleMatrixAsList: Vec<Vec<f64>> = callback("getDoubleMatrixAsList", &[]);
println!("getDoubleMatrixAsList: {:?}", getDoubleMatrixAsList);
if !getDoubleMatrixAsList[0][0].is_nan() {
    panic!("getDoubleMatrixAsList[0][0] not NaN!");
}
if !getDoubleMatrixAsList[1][1].is_nan() {
    panic!("getDoubleMatrixAsList[1][1] not NaN!");
}
if !getDoubleMatrixAsList[2][2].is_nan() {
    panic!("getDoubleMatrixAsList[2][2] not NaN!");
}
callback_void("setDoubleMatrixAsList", &[param(getDoubleMatrixAsList)]);
