println!("getByte");
let getByte: i32 = callback("getByte", &[]);
println!("getByte: {}", getByte);
callback_void("setByte", &[param(getByte)]);

println!("getByteVector");
let getByteVector: Vec<i32> = callback("getByteVector", &[]);
println!("getByteVector: {:?}", getByteVector);
callback_void("setByteVector", &[param(getByteVector)]);

println!("getByteVectorAsList");
let getByteVectorAsList: Vec<i32> = callback("getByteVectorAsList", &[]);
println!("getByteVectorAsList: {:?}", getByteVectorAsList);
callback_void("setByteVectorAsList", &[param(getByteVectorAsList)]);

println!("getByteMatrix");
let getByteMatrix: Vec<Vec<i32>> = callback("getByteMatrix", &[]);
println!("getByteMatrix: {:?}", getByteMatrix);
callback_void("setByteMatrix", &[param(getByteMatrix)]);

println!("getByteMatrixAsList");
let getByteMatrixAsList: Vec<Vec<i32>> = callback("getByteMatrixAsList", &[]);
println!("getByteMatrixAsList: {:?}", getByteMatrixAsList);
callback_void("setByteMatrixAsList", &[param(getByteMatrixAsList)]);
