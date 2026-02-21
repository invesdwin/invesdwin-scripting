println!("getFloat");
let getFloat: f64 = callback("getFloat", &[]);
println!("getFloat: {}", getFloat);
callback_void("setFloat", &[param(getFloat)]);

println!("getFloatVector");
let getFloatVector: Vec<f64> = callback("getFloatVector", &[]);
println!("getFloatVector: {:?}", getFloatVector);
callback_void("setFloatVector", &[param(getFloatVector)]);

println!("getFloatVectorAsList");
let getFloatVectorAsList: Vec<f64> = callback("getFloatVectorAsList", &[]);
println!("getFloatVectorAsList: {:?}", getFloatVectorAsList);
callback_void("setFloatVectorAsList", &[param(getFloatVectorAsList)]);

println!("getFloatMatrix");
let getFloatMatrix: Vec<Vec<f64>> = callback("getFloatMatrix", &[]);
println!("getFloatMatrix: {:?}", getFloatMatrix);
callback_void("setFloatMatrix", &[param(getFloatMatrix)]);

println!("getFloatMatrixAsList");
let getFloatMatrixAsList: Vec<Vec<f64>> = callback("getFloatMatrixAsList", &[]);
println!("getFloatMatrixAsList: {:?}", getFloatMatrixAsList);
callback_void("setFloatMatrixAsList", &[param(getFloatMatrixAsList)]);
