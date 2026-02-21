println!("getDouble");
let getDouble: f64 = callback("getDouble", &[]);
println!("getDouble: {}", getDouble);
callback_void("setDouble", &[param(getDouble)]);

println!("getDoubleVector");
let getDoubleVector: Vec<f64> = callback("getDoubleVector", &[]);
println!("getDoubleVector: {:?}", getDoubleVector);
callback_void("setDoubleVector", &[param(getDoubleVector)]);

println!("getDoubleVectorAsList");
let getDoubleVectorAsList: Vec<f64> = callback("getDoubleVectorAsList", &[]);
println!("getDoubleVectorAsList: {:?}", getDoubleVectorAsList);
callback_void("setDoubleVectorAsList", &[param(getDoubleVectorAsList)]);

println!("getDoubleMatrix");
let getDoubleMatrix: Vec<Vec<f64>> = callback("getDoubleMatrix", &[]);
println!("getDoubleMatrix: {:?}", getDoubleMatrix);
callback_void("setDoubleMatrix", &[param(getDoubleMatrix)]);

println!("getDoubleMatrixAsList");
let getDoubleMatrixAsList: Vec<Vec<f64>> = callback("getDoubleMatrixAsList", &[]);
println!("getDoubleMatrixAsList: {:?}", getDoubleMatrixAsList);
callback_void("setDoubleMatrixAsList", &[param(getDoubleMatrixAsList)]);
