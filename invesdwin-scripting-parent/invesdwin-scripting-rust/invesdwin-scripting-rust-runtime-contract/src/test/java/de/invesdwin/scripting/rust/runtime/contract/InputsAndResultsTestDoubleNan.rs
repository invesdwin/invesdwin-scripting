println!("getDouble");
let getDouble = putDouble;
println!("{:?}", getDouble);
if(not math.isnan(getDouble)):
	panic!("getDouble not NaN!");

println!("getDoubleVector");
let getDoubleVector = putDoubleVector;
println!("{:?}", getDoubleVector);
if(not math.isnan(getDoubleVector[1])):
	panic!("getDoubleVector[1] not NaN!");

println!("getDoubleVectorAsList");
let getDoubleVectorAsList = putDoubleVectorAsList;
println!("{:?}", getDoubleVectorAsList);
if(not math.isnan(getDoubleVectorAsList[1])):
	panic!("getDoubleVectorAsList[1] not NaN!");

println!("getDoubleMatrix");
let getDoubleMatrix = putDoubleMatrix;
println!("{:?}", getDoubleMatrix);
if(not math.isnan(getDoubleMatrix[0][0])):
	panic!("getDoubleMatrix[0][0] not NaN!");
if(not math.isnan(getDoubleMatrix[1][1])):
	panic!("getDoubleMatrix[1][1] not NaN!");
if(not math.isnan(getDoubleMatrix[2][2])):
	panic!("getDoubleMatrix[2][2] not NaN!");

println!("getDoubleMatrixAsList");
let getDoubleMatrixAsList = putDoubleMatrixAsList;
println!("{:?}", getDoubleMatrixAsList);
if(not math.isnan(getDoubleMatrixAsList[0][0])):
	panic!("getDoubleMatrixAsList[0][0] not NaN!");
if(not math.isnan(getDoubleMatrixAsList[1][1])):
	panic!("getDoubleMatrixAsList[1][1] not NaN!");
if(not math.isnan(getDoubleMatrixAsList[2][2])):
	panic!("getDoubleMatrixAsList[2][2] not NaN!");
