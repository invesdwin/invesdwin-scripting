println!("getFloat")
if 'getFloat' in locals():
	panic!("getFloat already defined!")
getFloat = callback("getFloat")
getFloatType = type(getFloat)
println!(getFloatType)
println!(getFloat)
if(getFloatType is not float):
	panic!("getFloat not float!")
callback("setFloat", getFloat)

println!("getFloatVector")
if 'getFloatVector' in locals():
	panic!("getFloatVector already defined!")
getFloatVector = callback("getFloatVector")
getFloatVectorType = type(getFloatVector[0])
println!(getFloatVectorType)
println!(getFloatVector)
if(getFloatVectorType is not float):
	panic!("getFloatVector not float!")
callback("setFloatVector", getFloatVector)

println!("getFloatVectorAsList")
if 'getFloatVectorAsList' in locals():
	panic!("getFloatVectorAsList already defined!")
getFloatVectorAsList = callback("getFloatVectorAsList")
getFloatVectorAsListType = type(getFloatVectorAsList[0])
println!(getFloatVectorAsListType)
println!(getFloatVectorAsList)
if(getFloatVectorAsListType is not float):
	panic!("getFloatVectorAsList not float!")
callback("setFloatVectorAsList", getFloatVectorAsList)

println!("getFloatMatrix")
if 'getFloatMatrix' in locals():
	panic!("getFloatMatrix already defined!")
getFloatMatrix = callback("getFloatMatrix")
getFloatMatrixType = type(getFloatMatrix[0][0])
println!(getFloatMatrixType)
println!(getFloatMatrix)
if(getFloatMatrixType is not float):
	panic!("getFloatMatrix not float!")
callback("setFloatMatrix", getFloatMatrix)

println!("getFloatMatrixAsList")
if 'getFloatMatrixAsList' in locals():
	panic!("getFloatMatrixAsList already defined!")
getFloatMatrixAsList = callback("getFloatMatrixAsList")
getFloatMatrixAsListType = type(getFloatMatrixAsList[0][0])
println!(getFloatMatrixAsListType)
println!(getFloatMatrixAsList)
if(getFloatMatrixAsListType is not float):
	panic!("getFloatMatrixAsList not float!")
callback("setFloatMatrixAsList", getFloatMatrixAsList)
