println!("getDouble");
let getDouble = putDouble;
println!("{:?}", getDouble);
if(not math.isnan(getDouble)):
	raise Exception("getDouble not NaN!");

println!("getDoubleVector");
let getDoubleVector = putDoubleVector;
println!("{:?}", getDoubleVector);
if(not math.isnan(getDoubleVector[1])):
	raise Exception("getDoubleVector[1] not NaN!");

println!("getDoubleVectorAsList");
let getDoubleVectorAsList = putDoubleVectorAsList;
println!("{:?}", getDoubleVectorAsList);
if(not math.isnan(getDoubleVectorAsList[1])):
	raise Exception("getDoubleVectorAsList[1] not NaN!");

println!("getDoubleMatrix");
let getDoubleMatrix = putDoubleMatrix;
println!("{:?}", getDoubleMatrix);
if(not math.isnan(getDoubleMatrix[0][0])):
	raise Exception("getDoubleMatrix[0][0] not NaN!");
if(not math.isnan(getDoubleMatrix[1][1])):
	raise Exception("getDoubleMatrix[1][1] not NaN!");
if(not math.isnan(getDoubleMatrix[2][2])):
	raise Exception("getDoubleMatrix[2][2] not NaN!");

println!("getDoubleMatrixAsList");
let getDoubleMatrixAsList = putDoubleMatrixAsList;
println!("{:?}", getDoubleMatrixAsList);
if(not math.isnan(getDoubleMatrixAsList[0][0])):
	raise Exception("getDoubleMatrixAsList[0][0] not NaN!");
if(not math.isnan(getDoubleMatrixAsList[1][1])):
	raise Exception("getDoubleMatrixAsList[1][1] not NaN!");
if(not math.isnan(getDoubleMatrixAsList[2][2])):
	raise Exception("getDoubleMatrixAsList[2][2] not NaN!");
