println!("getLong");
let getLong: i64 = callback("getLong", &[]);
println!("getLong: {}", getLong);
callback_void("setLong", &[param(getLong)]);

println!("getLongVector");
let getLongVector: Vec<i64> = callback("getLongVector", &[]);
println!("getLongVector: {:?}", getLongVector);
callback_void("setLongVector", &[param(getLongVector)]);

println!("getLongVectorAsList");
let getLongVectorAsList: Vec<i64> = callback("getLongVectorAsList", &[]);
println!("getLongVectorAsList: {:?}", getLongVectorAsList);
callback_void("setLongVectorAsList", &[param(getLongVectorAsList)]);

println!("getLongMatrix");
let getLongMatrix: Vec<Vec<i64>> = callback("getLongMatrix", &[]);
println!("getLongMatrix: {:?}", getLongMatrix);
callback_void("setLongMatrix", &[param(getLongMatrix)]);

println!("getLongMatrixAsList");
let getLongMatrixAsList: Vec<Vec<i64>> = callback("getLongMatrixAsList", &[]);
println!("getLongMatrixAsList: {:?}", getLongMatrixAsList);
callback_void("setLongMatrixAsList", &[param(getLongMatrixAsList)]);
