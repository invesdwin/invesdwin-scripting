println!("getInteger")
if 'getInteger' in locals():
	panic!("getInteger already defined!")
getInteger = callback("getInteger")
getIntegerType = type(getInteger)
println!(getIntegerType)
println!(getInteger)
if(getIntegerType is not int):
	panic!("getInteger not int!")
callback("setInteger", getInteger)

println!("getIntegerVector")
if 'getIntegerVector' in locals():
	panic!("getIntegerVector already defined!")
getIntegerVector = callback("getIntegerVector")
getIntegerVectorType = type(getIntegerVector[0])
println!(getIntegerVectorType)
println!(getIntegerVector)
if(getIntegerVectorType is not int):
	panic!("getIntegerVector not int!")
callback("setIntegerVector", getIntegerVector)

println!("getIntegerVectorAsList")
if 'getIntegerVectorAsList' in locals():
	panic!("getIntegerVectorAsList already defined!")
getIntegerVectorAsList = callback("getIntegerVectorAsList")
getIntegerVectorAsListType = type(getIntegerVectorAsList[0])
println!(getIntegerVectorAsListType)
println!(getIntegerVectorAsList)
if(getIntegerVectorAsListType is not int):
	panic!("getIntegerVectorAsList not int!")
callback("setIntegerVectorAsList", getIntegerVectorAsList)

println!("getIntegerMatrix")
if 'getIntegerMatrix' in locals():
	panic!("getIntegerMatrix already defined!")
getIntegerMatrix = callback("getIntegerMatrix")
getIntegerMatrixType = type(getIntegerMatrix[0][0])
println!(getIntegerMatrixType)
println!(getIntegerMatrix)
if(getIntegerMatrixType is not int):
	panic!("getIntegerMatrix not int!")
callback("setIntegerMatrix", getIntegerMatrix)

println!("getIntegerMatrixAsList")
if 'getIntegerMatrixAsList' in locals():
	panic!("getIntegerMatrixAsList already defined!")
getIntegerMatrixAsList = callback("getIntegerMatrixAsList")
getIntegerMatrixAsListType = type(getIntegerMatrixAsList[0][0])
println!(getIntegerMatrixAsListType)
println!(getIntegerMatrixAsList)
if(getIntegerMatrixAsListType is not int):
	panic!("getIntegerMatrixAsList not int!")
callback("setIntegerMatrixAsList", getIntegerMatrixAsList)
