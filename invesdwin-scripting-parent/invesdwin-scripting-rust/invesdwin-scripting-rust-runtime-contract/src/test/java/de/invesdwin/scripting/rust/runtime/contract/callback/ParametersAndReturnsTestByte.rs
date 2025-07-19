println!("getByte")
if 'getByte' in locals():
	panic!("getByte already defined!")
getByte = callback("getByte")
getByteType = type(getByte)
println!(getByteType)
println!(getByte)
if(getByteType is not int):
	panic!("getByte not int!")
callback("setByte", getByte)

println!("getByteVector")
if 'getByteVector' in locals():
	panic!("getByteVector already defined!")
getByteVector = callback("getByteVector")
getByteVectorType = type(getByteVector[0])
println!(getByteVectorType)
println!(getByteVector)
if(getByteVectorType is not int):
	panic!("getByteVector not int!")
callback("setByteVector", getByteVector)

println!("getByteVectorAsList")
if 'getByteVectorAsList' in locals():
	panic!("getByteVectorAsList already defined!")
getByteVectorAsList = callback("getByteVectorAsList")
getByteVectorAsListType = type(getByteVectorAsList[0])
println!(getByteVectorAsListType)
println!(getByteVectorAsList)
if(getByteVectorAsListType is not int):
	panic!("getByteVectorAsList not int!")
callback("setByteVectorAsList", getByteVectorAsList)

println!("getByteMatrix")
if 'getByteMatrix' in locals():
	panic!("getByteMatrix already defined!")
getByteMatrix = callback("getByteMatrix")
getByteMatrixType = type(getByteMatrix[0][0])
println!(getByteMatrixType)
println!(getByteMatrix)
if(getByteMatrixType is not int):
	panic!("getByteMatrix not int!")
callback("setByteMatrix", getByteMatrix)

println!("getByteMatrixAsList")
if 'getByteMatrixAsList' in locals():
	panic!("getByteMatrixAsList already defined!")
getByteMatrixAsList = callback("getByteMatrixAsList")
getByteMatrixAsListType = type(getByteMatrixAsList[0][0])
println!(getByteMatrixAsListType)
println!(getByteMatrixAsList)
if(getByteMatrixAsListType is not int):
	panic!("getByteMatrixAsList not int!")
callback("setByteMatrixAsList", getByteMatrixAsList)
