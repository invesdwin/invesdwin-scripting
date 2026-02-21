println!("getDecimal");
let getDecimal: f64 = callback("getDecimal", &[]);
println!("getDecimal: {}", getDecimal);
callback_void("setDecimal", &[param(getDecimal)]);

println!("getDecimalVector");
let getDecimalVector: Vec<f64> = callback("getDecimalVector", &[]);
println!("getDecimalVector: {:?}", getDecimalVector);
callback_void("setDecimalVector", &[param(getDecimalVector)]);

println!("getDecimalVectorAsList");
let getDecimalVectorAsList: Vec<f64> = callback("getDecimalVectorAsList", &[]);
println!("getDecimalVectorAsList: {:?}", getDecimalVectorAsList);
callback_void("setDecimalVectorAsList", &[param(getDecimalVectorAsList)]);

println!("getDecimalMatrix");
let getDecimalMatrix: Vec<Vec<f64>> = callback("getDecimalMatrix", &[]);
println!("getDecimalMatrix: {:?}", getDecimalMatrix);
callback_void("setDecimalMatrix", &[param(getDecimalMatrix)]);

println!("getDecimalMatrixAsList");
let getDecimalMatrixAsList: Vec<Vec<f64>> = callback("getDecimalMatrixAsList", &[]);
println!("getDecimalMatrixAsList: {:?}", getDecimalMatrixAsList);
callback_void("setDecimalMatrixAsList", &[param(getDecimalMatrixAsList)]);
