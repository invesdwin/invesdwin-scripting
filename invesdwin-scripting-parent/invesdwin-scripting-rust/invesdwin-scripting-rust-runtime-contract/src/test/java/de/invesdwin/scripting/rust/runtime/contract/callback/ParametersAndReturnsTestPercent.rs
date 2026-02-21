println!("getPercent");
let getPercent: f64 = callback("getPercent", &[]);
println!("getPercent: {}", getPercent);
callback_void("setPercent", &[param(getPercent)]);

println!("getPercentVector");
let getPercentVector: Vec<f64> = callback("getPercentVector", &[]);
println!("getPercentVector: {:?}", getPercentVector);
callback_void("setPercentVector", &[param(getPercentVector)]);

println!("getPercentVectorAsList");
let getPercentVectorAsList: Vec<f64> = callback("getPercentVectorAsList", &[]);
println!("getPercentVectorAsList: {:?}", getPercentVectorAsList);
callback_void("setPercentVectorAsList", &[param(getPercentVectorAsList)]);

println!("getPercentMatrix");
let getPercentMatrix: Vec<Vec<f64>> = callback("getPercentMatrix", &[]);
println!("getPercentMatrix: {:?}", getPercentMatrix);
callback_void("setPercentMatrix", &[param(getPercentMatrix)]);

println!("getPercentMatrixAsList");
let getPercentMatrixAsList: Vec<Vec<f64>> = callback("getPercentMatrixAsList", &[]);
println!("getPercentMatrixAsList: {:?}", getPercentMatrixAsList);
callback_void("setPercentMatrixAsList", &[param(getPercentMatrixAsList)]);
