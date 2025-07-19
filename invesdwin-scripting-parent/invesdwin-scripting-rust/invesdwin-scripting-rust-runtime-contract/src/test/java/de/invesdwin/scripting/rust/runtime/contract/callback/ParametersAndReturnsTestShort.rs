println!("getShort")
if 'getShort' in locals():
	panic!("getShort already defined!")
getShort = callback("getShort")
getShortType = type(getShort)
println!(getShortType)
println!(getShort)
if(getShortType is not int):
	panic!("getShort not int!")
callback("setShort", getShort)

println!("getShortVector")
if 'getShortVector' in locals():
	panic!("getShortVector already defined!")
getShortVector = callback("getShortVector")
getShortVectorType = type(getShortVector[0])
println!(getShortVectorType)
println!(getShortVector)
if(getShortVectorType is not int):
	panic!("getShortVector not int!")
callback("setShortVector", getShortVector)

println!("getShortVectorAsList")
if 'getShortVectorAsList' in locals():
	panic!("getShortVectorAsList already defined!")
getShortVectorAsList = callback("getShortVectorAsList")
getShortVectorAsListType = type(getShortVectorAsList[0])
println!(getShortVectorAsListType)
println!(getShortVectorAsList)
if(getShortVectorAsListType is not int):
	panic!("getShortVectorAsList not int!")
callback("setShortVectorAsList", getShortVectorAsList)

println!("getShortMatrix")
if 'getShortMatrix' in locals():
	panic!("getShortMatrix already defined!")
getShortMatrix = callback("getShortMatrix")
getShortMatrixType = type(getShortMatrix[0][0])
println!(getShortMatrixType)
println!(getShortMatrix)
if(getShortMatrixType is not int):
	panic!("getShortMatrix not int!")
callback("setShortMatrix", getShortMatrix)

println!("getShortMatrixAsList")
if 'getShortMatrixAsList' in locals():
	panic!("getShortMatrixAsList already defined!")
getShortMatrixAsList = callback("getShortMatrixAsList")
getShortMatrixAsListType = type(getShortMatrixAsList[0][0])
println!(getShortMatrixAsListType)
println!(getShortMatrixAsList)
if(getShortMatrixAsListType is not int):
	panic!("getShortMatrixAsList not int!")
callback("setShortMatrixAsList", getShortMatrixAsList)
