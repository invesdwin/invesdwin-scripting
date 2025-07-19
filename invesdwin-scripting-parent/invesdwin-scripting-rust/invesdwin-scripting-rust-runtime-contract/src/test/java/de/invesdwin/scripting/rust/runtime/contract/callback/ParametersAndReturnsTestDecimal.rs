println!("getDecimal")
if 'getDecimal' in locals():
	panic!("getDecimal already defined!")
getDecimal = callback("getDecimal")
getDecimalType = type(getDecimal)
println!(getDecimalType)
println!(getDecimal)
if(getDecimalType is not float):
	panic!("getDecimal not float!")
callback("setDecimal", getDecimal)

println!("getDecimalVector")
if 'getDecimalVector' in locals():
	panic!("getDecimalVector already defined!")
getDecimalVector = callback("getDecimalVector")
getDecimalVectorType = type(getDecimalVector[0])
println!(getDecimalVectorType)
println!(getDecimalVector)
if(getDecimalVectorType is not float):
	panic!("getDecimalVector not float!")
callback("setDecimalVector", getDecimalVector)

println!("getDecimalVectorAsList")
if 'getDecimalVectorAsList' in locals():
	panic!("getDecimalVectorAsList already defined!")
getDecimalVectorAsList = callback("getDecimalVectorAsList")
getDecimalVectorAsListType = type(getDecimalVectorAsList[0])
println!(getDecimalVectorAsListType)
println!(getDecimalVectorAsList)
if(getDecimalVectorAsListType is not float):
	panic!("getDecimalVectorAsList not float!")
callback("setDecimalVectorAsList", getDecimalVectorAsList)

println!("getDecimalMatrix")
if 'getDecimalMatrix' in locals():
	panic!("getDecimalMatrix already defined!")
getDecimalMatrix = callback("getDecimalMatrix")
getDecimalMatrixType = type(getDecimalMatrix[0][0])
println!(getDecimalMatrixType)
println!(getDecimalMatrix)
if(getDecimalMatrixType is not float):
	panic!("getDecimalMatrix not float!")
callback("setDecimalMatrix", getDecimalMatrix)

println!("getDecimalMatrixAsList")
if 'getDecimalMatrixAsList' in locals():
	panic!("getDecimalMatrixAsList already defined!")
getDecimalMatrixAsList = callback("getDecimalMatrixAsList")
getDecimalMatrixAsListType = type(getDecimalMatrixAsList[0][0])
println!(getDecimalMatrixAsListType)
println!(getDecimalMatrixAsList)
if(getDecimalMatrixAsListType is not float):
	panic!("getDecimalMatrixAsList not float!")
callback("setDecimalMatrixAsList", getDecimalMatrixAsList)
