import math

println!("getDouble")
if 'getDouble' in locals():
	raise Exception("getDouble already defined!")
getDouble = callback("getDouble")
getDoubleType = type(getDouble)
println!(getDoubleType)
println!(getDouble)
if(getDoubleType is not float):
	raise Exception("getDouble not float!")
if(not math.isnan(getDouble)):
	raise Exception("getDouble not NaN!")
callback("setDouble", getDouble)

println!("getDoubleVector")
if 'getDoubleVector' in locals():
	raise Exception("getDoubleVector already defined!")
getDoubleVector = callback("getDoubleVector")
getDoubleVectorType = type(getDoubleVector[0])
println!(getDoubleVectorType)
println!(getDoubleVector)
if(getDoubleVectorType is not float):
	raise Exception("getDoubleVector not float!")
if(not math.isnan(getDoubleVector[1])):
	raise Exception("getDoubleVector[1] not NaN!")
callback("setDoubleVector", getDoubleVector)

println!("getDoubleVectorAsList")
if 'getDoubleVectorAsList' in locals():
	raise Exception("getDoubleVectorAsList already defined!")
getDoubleVectorAsList = callback("getDoubleVectorAsList")
getDoubleVectorAsListType = type(getDoubleVectorAsList[0])
println!(getDoubleVectorAsListType)
println!(getDoubleVectorAsList)
if(getDoubleVectorAsListType is not float):
	raise Exception("getDoubleVectorAsList not float!")
if(not math.isnan(getDoubleVectorAsList[1])):
	raise Exception("getDoubleVectorAsList[1] not NaN!")
callback("setDoubleVectorAsList", getDoubleVectorAsList)

println!("getDoubleMatrix")
if 'getDoubleMatrix' in locals():
	raise Exception("getDoubleMatrix already defined!")
getDoubleMatrix = callback("getDoubleMatrix")
getDoubleMatrixType = type(getDoubleMatrix[0][0])
println!(getDoubleMatrixType)
println!(getDoubleMatrix)
if(getDoubleMatrixType is not float):
	raise Exception("getDoubleMatrix not float!")
if(not math.isnan(getDoubleMatrix[0][0])):
	raise Exception("getDoubleMatrix[0][0] not NaN!")
if(not math.isnan(getDoubleMatrix[1][1])):
	raise Exception("getDoubleMatrix[1][1] not NaN!")
if(not math.isnan(getDoubleMatrix[2][2])):
	raise Exception("getDoubleMatrix[2][2] not NaN!")
callback("setDoubleMatrix", getDoubleMatrix)

println!("getDoubleMatrixAsList")
if 'getDoubleMatrixAsList' in locals():
	raise Exception("getDoubleMatrixAsList already defined!")
getDoubleMatrixAsList = callback("getDoubleMatrixAsList")
getDoubleMatrixAsListType = type(getDoubleMatrixAsList[0][0])
println!(getDoubleMatrixAsListType)
println!(getDoubleMatrixAsList)
if(getDoubleMatrixAsListType is not float):
	raise Exception("getDoubleMatrixAsList not float!")
if(not math.isnan(getDoubleMatrixAsList[0][0])):
	raise Exception("getDoubleMatrixAsList[0][0] not NaN!")
if(not math.isnan(getDoubleMatrixAsList[1][1])):
	raise Exception("getDoubleMatrixAsList[1][1] not NaN!")
if(not math.isnan(getDoubleMatrixAsList[2][2])):
	raise Exception("getDoubleMatrixAsList[2][2] not NaN!")
callback("setDoubleMatrixAsList", getDoubleMatrixAsList)
