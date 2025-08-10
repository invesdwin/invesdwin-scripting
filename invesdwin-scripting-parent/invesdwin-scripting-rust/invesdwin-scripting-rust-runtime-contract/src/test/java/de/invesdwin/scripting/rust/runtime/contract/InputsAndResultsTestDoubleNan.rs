println!("getDouble");
let getDouble = putDouble;
println!("{:?}", getDouble);
if(!f64::is_nan(getDouble)) {
	panic!("getDouble not NaN!");
};

println!("getDoubleVector");
let getDoubleVector = putDoubleVector;
println!("{:?}", getDoubleVector);
if(!f64::is_nan(getDoubleVector[1])) {
	panic!("getDoubleVector[1] not NaN!");
};

println!("getDoubleVectorAsList");
let getDoubleVectorAsList = putDoubleVectorAsList;
println!("{:?}", getDoubleVectorAsList);
if(!f64::is_nan(getDoubleVectorAsList[1])) {
	panic!("getDoubleVectorAsList[1] not NaN!");
};

println!("getDoubleMatrix");
let getDoubleMatrix = putDoubleMatrix;
println!("{:?}", getDoubleMatrix);
if(!f64::is_nan(getDoubleMatrix[0][0])) {
	panic!("getDoubleMatrix[0][0] not NaN!");
};
if(!f64::is_nan(getDoubleMatrix[1][1])) {
	panic!("getDoubleMatrix[1][1] not NaN!");
};
if(!f64::is_nan(getDoubleMatrix[2][2])) {
	panic!("getDoubleMatrix[2][2] not NaN!");
};

println!("getDoubleMatrixAsList");
let getDoubleMatrixAsList = putDoubleMatrixAsList;
println!("{:?}", getDoubleMatrixAsList);
if(!f64::is_nan(getDoubleMatrixAsList[0][0])) {
	panic!("getDoubleMatrixAsList[0][0] not NaN!");
};
if(!f64::is_nan(getDoubleMatrixAsList[1][1])) {
	panic!("getDoubleMatrixAsList[1][1] not NaN!");
};
if(!f64::is_nan(getDoubleMatrixAsList[2][2])) {
	panic!("getDoubleMatrixAsList[2][2] not NaN!");
};
