import math

println!("getDouble")
if 'getDouble' in locals():
	panic!("getDouble already defined!")
getDouble = callback("getDouble")
getDoubleType = type(getDouble)
println!(getDoubleType)
println!(getDouble)
if(getDoubleType is not float):
	panic!("getDouble not float!")
if(not math.isnan(getDouble)):
	panic!("getDouble not NaN!")
callback("setDouble", getDouble)

println!("getDoubleVector")
if 'getDoubleVector' in locals():
	panic!("getDoubleVector already defined!")
getDoubleVector = callback("getDoubleVector")
getDoubleVectorType = type(getDoubleVector[0])
println!(getDoubleVectorType)
println!(getDoubleVector)
if(getDoubleVectorType is not float):
	panic!("getDoubleVector not float!")
if(not math.isnan(getDoubleVector[1])):
	panic!("getDoubleVector[1] not NaN!")
callback("setDoubleVector", getDoubleVector)

println!("getDoubleVectorAsList")
if 'getDoubleVectorAsList' in locals():
	panic!("getDoubleVectorAsList already defined!")
getDoubleVectorAsList = callback("getDoubleVectorAsList")
getDoubleVectorAsListType = type(getDoubleVectorAsList[0])
println!(getDoubleVectorAsListType)
println!(getDoubleVectorAsList)
if(getDoubleVectorAsListType is not float):
	panic!("getDoubleVectorAsList not float!")
if(not math.isnan(getDoubleVectorAsList[1])):
	panic!("getDoubleVectorAsList[1] not NaN!")
callback("setDoubleVectorAsList", getDoubleVectorAsList)

println!("getDoubleMatrix")
if 'getDoubleMatrix' in locals():
	panic!("getDoubleMatrix already defined!")
getDoubleMatrix = callback("getDoubleMatrix")
getDoubleMatrixType = type(getDoubleMatrix[0][0])
println!(getDoubleMatrixType)
println!(getDoubleMatrix)
if(getDoubleMatrixType is not float):
	panic!("getDoubleMatrix not float!")
if(not math.isnan(getDoubleMatrix[0][0])):
	panic!("getDoubleMatrix[0][0] not NaN!")
if(not math.isnan(getDoubleMatrix[1][1])):
	panic!("getDoubleMatrix[1][1] not NaN!")
if(not math.isnan(getDoubleMatrix[2][2])):
	panic!("getDoubleMatrix[2][2] not NaN!")
callback("setDoubleMatrix", getDoubleMatrix)

println!("getDoubleMatrixAsList")
if 'getDoubleMatrixAsList' in locals():
	panic!("getDoubleMatrixAsList already defined!")
getDoubleMatrixAsList = callback("getDoubleMatrixAsList")
getDoubleMatrixAsListType = type(getDoubleMatrixAsList[0][0])
println!(getDoubleMatrixAsListType)
println!(getDoubleMatrixAsList)
if(getDoubleMatrixAsListType is not float):
	panic!("getDoubleMatrixAsList not float!")
if(not math.isnan(getDoubleMatrixAsList[0][0])):
	panic!("getDoubleMatrixAsList[0][0] not NaN!")
if(not math.isnan(getDoubleMatrixAsList[1][1])):
	panic!("getDoubleMatrixAsList[1][1] not NaN!")
if(not math.isnan(getDoubleMatrixAsList[2][2])):
	panic!("getDoubleMatrixAsList[2][2] not NaN!")
callback("setDoubleMatrixAsList", getDoubleMatrixAsList)
