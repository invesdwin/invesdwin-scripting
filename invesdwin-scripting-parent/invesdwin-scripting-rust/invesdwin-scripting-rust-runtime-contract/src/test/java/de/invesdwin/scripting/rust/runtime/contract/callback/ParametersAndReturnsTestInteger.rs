println!("getInteger")
if 'getInteger' in locals():
	raise Exception("getInteger already defined!")
getInteger = callback("getInteger")
getIntegerType = type(getInteger)
println!(getIntegerType)
println!(getInteger)
if(getIntegerType is not int):
	raise Exception("getInteger not int!")
callback("setInteger", getInteger)

println!("getIntegerVector")
if 'getIntegerVector' in locals():
	raise Exception("getIntegerVector already defined!")
getIntegerVector = callback("getIntegerVector")
getIntegerVectorType = type(getIntegerVector[0])
println!(getIntegerVectorType)
println!(getIntegerVector)
if(getIntegerVectorType is not int):
	raise Exception("getIntegerVector not int!")
callback("setIntegerVector", getIntegerVector)

println!("getIntegerVectorAsList")
if 'getIntegerVectorAsList' in locals():
	raise Exception("getIntegerVectorAsList already defined!")
getIntegerVectorAsList = callback("getIntegerVectorAsList")
getIntegerVectorAsListType = type(getIntegerVectorAsList[0])
println!(getIntegerVectorAsListType)
println!(getIntegerVectorAsList)
if(getIntegerVectorAsListType is not int):
	raise Exception("getIntegerVectorAsList not int!")
callback("setIntegerVectorAsList", getIntegerVectorAsList)

println!("getIntegerMatrix")
if 'getIntegerMatrix' in locals():
	raise Exception("getIntegerMatrix already defined!")
getIntegerMatrix = callback("getIntegerMatrix")
getIntegerMatrixType = type(getIntegerMatrix[0][0])
println!(getIntegerMatrixType)
println!(getIntegerMatrix)
if(getIntegerMatrixType is not int):
	raise Exception("getIntegerMatrix not int!")
callback("setIntegerMatrix", getIntegerMatrix)

println!("getIntegerMatrixAsList")
if 'getIntegerMatrixAsList' in locals():
	raise Exception("getIntegerMatrixAsList already defined!")
getIntegerMatrixAsList = callback("getIntegerMatrixAsList")
getIntegerMatrixAsListType = type(getIntegerMatrixAsList[0][0])
println!(getIntegerMatrixAsListType)
println!(getIntegerMatrixAsList)
if(getIntegerMatrixAsListType is not int):
	raise Exception("getIntegerMatrixAsList not int!")
callback("setIntegerMatrixAsList", getIntegerMatrixAsList)
