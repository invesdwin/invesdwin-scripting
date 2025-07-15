println!("getDecimal")
if 'getDecimal' in locals():
	raise Exception("getDecimal already defined!")
getDecimal = callback("getDecimal")
getDecimalType = type(getDecimal)
println!(getDecimalType)
println!(getDecimal)
if(getDecimalType is not float):
	raise Exception("getDecimal not float!")
callback("setDecimal", getDecimal)

println!("getDecimalVector")
if 'getDecimalVector' in locals():
	raise Exception("getDecimalVector already defined!")
getDecimalVector = callback("getDecimalVector")
getDecimalVectorType = type(getDecimalVector[0])
println!(getDecimalVectorType)
println!(getDecimalVector)
if(getDecimalVectorType is not float):
	raise Exception("getDecimalVector not float!")
callback("setDecimalVector", getDecimalVector)

println!("getDecimalVectorAsList")
if 'getDecimalVectorAsList' in locals():
	raise Exception("getDecimalVectorAsList already defined!")
getDecimalVectorAsList = callback("getDecimalVectorAsList")
getDecimalVectorAsListType = type(getDecimalVectorAsList[0])
println!(getDecimalVectorAsListType)
println!(getDecimalVectorAsList)
if(getDecimalVectorAsListType is not float):
	raise Exception("getDecimalVectorAsList not float!")
callback("setDecimalVectorAsList", getDecimalVectorAsList)

println!("getDecimalMatrix")
if 'getDecimalMatrix' in locals():
	raise Exception("getDecimalMatrix already defined!")
getDecimalMatrix = callback("getDecimalMatrix")
getDecimalMatrixType = type(getDecimalMatrix[0][0])
println!(getDecimalMatrixType)
println!(getDecimalMatrix)
if(getDecimalMatrixType is not float):
	raise Exception("getDecimalMatrix not float!")
callback("setDecimalMatrix", getDecimalMatrix)

println!("getDecimalMatrixAsList")
if 'getDecimalMatrixAsList' in locals():
	raise Exception("getDecimalMatrixAsList already defined!")
getDecimalMatrixAsList = callback("getDecimalMatrixAsList")
getDecimalMatrixAsListType = type(getDecimalMatrixAsList[0][0])
println!(getDecimalMatrixAsListType)
println!(getDecimalMatrixAsList)
if(getDecimalMatrixAsListType is not float):
	raise Exception("getDecimalMatrixAsList not float!")
callback("setDecimalMatrixAsList", getDecimalMatrixAsList)
