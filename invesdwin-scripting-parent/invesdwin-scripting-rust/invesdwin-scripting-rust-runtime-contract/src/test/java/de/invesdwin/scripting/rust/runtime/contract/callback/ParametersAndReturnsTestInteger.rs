println!("getInteger");
let getInteger: i32 = callback("getInteger", &[]);
println!("getInteger: {}", getInteger);
callback_void("setInteger", &[param(getInteger)]);

println!("getIntegerVector");
let getIntegerVector: Vec<i32> = callback("getIntegerVector", &[]);
println!("getIntegerVector: {:?}", getIntegerVector);
callback_void("setIntegerVector", &[param(getIntegerVector)]);

println!("getIntegerVectorAsList");
let getIntegerVectorAsList: Vec<i32> = callback("getIntegerVectorAsList", &[]);
println!("getIntegerVectorAsList: {:?}", getIntegerVectorAsList);
callback_void("setIntegerVectorAsList", &[param(getIntegerVectorAsList)]);

println!("getIntegerMatrix");
let getIntegerMatrix: Vec<Vec<i32>> = callback("getIntegerMatrix", &[]);
println!("getIntegerMatrix: {:?}", getIntegerMatrix);
callback_void("setIntegerMatrix", &[param(getIntegerMatrix)]);

println!("getIntegerMatrixAsList");
let getIntegerMatrixAsList: Vec<Vec<i32>> = callback("getIntegerMatrixAsList", &[]);
println!("getIntegerMatrixAsList: {:?}", getIntegerMatrixAsList);
callback_void("setIntegerMatrixAsList", &[param(getIntegerMatrixAsList)]);
