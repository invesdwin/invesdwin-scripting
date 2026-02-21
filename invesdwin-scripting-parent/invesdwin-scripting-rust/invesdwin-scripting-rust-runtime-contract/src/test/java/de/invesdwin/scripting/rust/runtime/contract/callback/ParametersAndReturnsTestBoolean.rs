println!("getBoolean");
let getBoolean: bool = callback("getBoolean", &[]);
println!("getBoolean: {}", getBoolean);
callback_void("setBoolean", &[param(getBoolean)]);

println!("getBooleanVector");
let getBooleanVector: Vec<bool> = callback("getBooleanVector", &[]);
println!("getBooleanVector: {:?}", getBooleanVector);
callback_void("setBooleanVector", &[param(getBooleanVector)]);

println!("getBooleanVectorAsList");
let getBooleanVectorAsList: Vec<bool> = callback("getBooleanVectorAsList", &[]);
println!("getBooleanVectorAsList: {:?}", getBooleanVectorAsList);
callback_void("setBooleanVectorAsList", &[param(getBooleanVectorAsList)]);

println!("getBooleanMatrix");
let getBooleanMatrix: Vec<Vec<bool>> = callback("getBooleanMatrix", &[]);
println!("getBooleanMatrix: {:?}", getBooleanMatrix);
callback_void("setBooleanMatrix", &[param(getBooleanMatrix)]);

println!("getBooleanMatrixAsList");
let getBooleanMatrixAsList: Vec<Vec<bool>> = callback("getBooleanMatrixAsList", &[]);
println!("getBooleanMatrixAsList: {:?}", getBooleanMatrixAsList);
callback_void("setBooleanMatrixAsList", &[param(getBooleanMatrixAsList)]);
