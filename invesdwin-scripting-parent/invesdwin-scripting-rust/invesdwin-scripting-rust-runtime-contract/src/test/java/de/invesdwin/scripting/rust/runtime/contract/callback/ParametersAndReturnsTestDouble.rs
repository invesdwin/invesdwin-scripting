println!("getDouble")
if 'getDouble' in locals():
	panic!("getDouble already defined!")
getDouble = callback("getDouble")
getDoubleType = type(getDouble)
println!(getDoubleType)
println!(getDouble)
if(getDoubleType is not float):
	panic!("getDouble not float!")
callback("setDouble", getDouble)

println!("getDoubleVector")
if 'getDoubleVector' in locals():
	panic!("getDoubleVector already defined!")
getDoubleVector = callback("getDoubleVector")
getDoubleVectorType = type(getDoubleVector[0])
println!(getDoubleVectorType)
println!(getDoubleVector)
if(getDoubleVectorType is not float):
	panic!("getDoubleVector not float!")
callback("setDoubleVector", getDoubleVector)

println!("getDoubleVectorAsList")
if 'getDoubleVectorAsList' in locals():
	panic!("getDoubleVectorAsList already defined!")
getDoubleVectorAsList = callback("getDoubleVectorAsList")
getDoubleVectorAsListType = type(getDoubleVectorAsList[0])
println!(getDoubleVectorAsListType)
println!(getDoubleVectorAsList)
if(getDoubleVectorAsListType is not float):
	panic!("getDoubleVectorAsList not float!")
callback("setDoubleVectorAsList", getDoubleVectorAsList)

println!("getDoubleMatrix")
if 'getDoubleMatrix' in locals():
	panic!("getDoubleMatrix already defined!")
getDoubleMatrix = callback("getDoubleMatrix")
getDoubleMatrixType = type(getDoubleMatrix[0][0])
println!(getDoubleMatrixType)
println!(getDoubleMatrix)
if(getDoubleMatrixType is not float):
	panic!("getDoubleMatrix not float!")
callback("setDoubleMatrix", getDoubleMatrix)

println!("getDoubleMatrixAsList")
if 'getDoubleMatrixAsList' in locals():
	panic!("getDoubleMatrixAsList already defined!")
getDoubleMatrixAsList = callback("getDoubleMatrixAsList")
getDoubleMatrixAsListType = type(getDoubleMatrixAsList[0][0])
println!(getDoubleMatrixAsListType)
println!(getDoubleMatrixAsList)
if(getDoubleMatrixAsListType is not float):
	panic!("getDoubleMatrixAsList not float!")
callback("setDoubleMatrixAsList", getDoubleMatrixAsList)
