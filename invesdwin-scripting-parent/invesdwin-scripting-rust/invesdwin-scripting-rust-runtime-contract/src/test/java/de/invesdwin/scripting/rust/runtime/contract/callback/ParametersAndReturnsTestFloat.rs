println!("getFloat")
if 'getFloat' in locals():
	raise Exception("getFloat already defined!")
getFloat = callback("getFloat")
getFloatType = type(getFloat)
println!(getFloatType)
println!(getFloat)
if(getFloatType is not float):
	raise Exception("getFloat not float!")
callback("setFloat", getFloat)

println!("getFloatVector")
if 'getFloatVector' in locals():
	raise Exception("getFloatVector already defined!")
getFloatVector = callback("getFloatVector")
getFloatVectorType = type(getFloatVector[0])
println!(getFloatVectorType)
println!(getFloatVector)
if(getFloatVectorType is not float):
	raise Exception("getFloatVector not float!")
callback("setFloatVector", getFloatVector)

println!("getFloatVectorAsList")
if 'getFloatVectorAsList' in locals():
	raise Exception("getFloatVectorAsList already defined!")
getFloatVectorAsList = callback("getFloatVectorAsList")
getFloatVectorAsListType = type(getFloatVectorAsList[0])
println!(getFloatVectorAsListType)
println!(getFloatVectorAsList)
if(getFloatVectorAsListType is not float):
	raise Exception("getFloatVectorAsList not float!")
callback("setFloatVectorAsList", getFloatVectorAsList)

println!("getFloatMatrix")
if 'getFloatMatrix' in locals():
	raise Exception("getFloatMatrix already defined!")
getFloatMatrix = callback("getFloatMatrix")
getFloatMatrixType = type(getFloatMatrix[0][0])
println!(getFloatMatrixType)
println!(getFloatMatrix)
if(getFloatMatrixType is not float):
	raise Exception("getFloatMatrix not float!")
callback("setFloatMatrix", getFloatMatrix)

println!("getFloatMatrixAsList")
if 'getFloatMatrixAsList' in locals():
	raise Exception("getFloatMatrixAsList already defined!")
getFloatMatrixAsList = callback("getFloatMatrixAsList")
getFloatMatrixAsListType = type(getFloatMatrixAsList[0][0])
println!(getFloatMatrixAsListType)
println!(getFloatMatrixAsList)
if(getFloatMatrixAsListType is not float):
	raise Exception("getFloatMatrixAsList not float!")
callback("setFloatMatrixAsList", getFloatMatrixAsList)
