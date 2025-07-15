println!("getShort")
if 'getShort' in locals():
	raise Exception("getShort already defined!")
getShort = callback("getShort")
getShortType = type(getShort)
println!(getShortType)
println!(getShort)
if(getShortType is not int):
	raise Exception("getShort not int!")
callback("setShort", getShort)

println!("getShortVector")
if 'getShortVector' in locals():
	raise Exception("getShortVector already defined!")
getShortVector = callback("getShortVector")
getShortVectorType = type(getShortVector[0])
println!(getShortVectorType)
println!(getShortVector)
if(getShortVectorType is not int):
	raise Exception("getShortVector not int!")
callback("setShortVector", getShortVector)

println!("getShortVectorAsList")
if 'getShortVectorAsList' in locals():
	raise Exception("getShortVectorAsList already defined!")
getShortVectorAsList = callback("getShortVectorAsList")
getShortVectorAsListType = type(getShortVectorAsList[0])
println!(getShortVectorAsListType)
println!(getShortVectorAsList)
if(getShortVectorAsListType is not int):
	raise Exception("getShortVectorAsList not int!")
callback("setShortVectorAsList", getShortVectorAsList)

println!("getShortMatrix")
if 'getShortMatrix' in locals():
	raise Exception("getShortMatrix already defined!")
getShortMatrix = callback("getShortMatrix")
getShortMatrixType = type(getShortMatrix[0][0])
println!(getShortMatrixType)
println!(getShortMatrix)
if(getShortMatrixType is not int):
	raise Exception("getShortMatrix not int!")
callback("setShortMatrix", getShortMatrix)

println!("getShortMatrixAsList")
if 'getShortMatrixAsList' in locals():
	raise Exception("getShortMatrixAsList already defined!")
getShortMatrixAsList = callback("getShortMatrixAsList")
getShortMatrixAsListType = type(getShortMatrixAsList[0][0])
println!(getShortMatrixAsListType)
println!(getShortMatrixAsList)
if(getShortMatrixAsListType is not int):
	raise Exception("getShortMatrixAsList not int!")
callback("setShortMatrixAsList", getShortMatrixAsList)
