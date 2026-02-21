println!("getShort");
let getShort: i16 = callback("getShort", &[]);
println!("getShort: {}", getShort);
callback_void("setShort", &[param(getShort)]);

println!("getShortVector");
let getShortVector: Vec<i16> = callback("getShortVector", &[]);
println!("getShortVector: {:?}", getShortVector);
callback_void("setShortVector", &[param(getShortVector)]);

println!("getShortVectorAsList");
let getShortVectorAsList: Vec<i16> = callback("getShortVectorAsList", &[]);
println!("getShortVectorAsList: {:?}", getShortVectorAsList);
callback_void("setShortVectorAsList", &[param(getShortVectorAsList)]);

println!("getShortMatrix");
let getShortMatrix: Vec<Vec<i16>> = callback("getShortMatrix", &[]);
println!("getShortMatrix: {:?}", getShortMatrix);
callback_void("setShortMatrix", &[param(getShortMatrix)]);

println!("getShortMatrixAsList");
let getShortMatrixAsList: Vec<Vec<i16>> = callback("getShortMatrixAsList", &[]);
println!("getShortMatrixAsList: {:?}", getShortMatrixAsList);
callback_void("setShortMatrixAsList", &[param(getShortMatrixAsList)]);
