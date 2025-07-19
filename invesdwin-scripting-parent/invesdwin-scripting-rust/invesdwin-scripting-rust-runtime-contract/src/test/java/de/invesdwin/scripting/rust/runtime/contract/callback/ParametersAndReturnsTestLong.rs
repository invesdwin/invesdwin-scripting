println!("getLong")
if 'getLong' in locals():
	panic!("getLong already defined!")
getLong = callback("getLong")
getLongType = type(getLong)
println!(getLongType)
println!(getLong)
if(getLongType is not long):
	panic!("getLong not long!")
callback("setLong", getLong)

println!("getLongVector")
if 'getLongVector' in locals():
	panic!("getLongVector already defined!")
getLongVector = callback("getLongVector")
getLongVectorType = type(getLongVector[0])
println!(getLongVectorType)
println!(getLongVector)
if(getLongVectorType is not long):
	panic!("getLongVector not long!")
callback("setLongVector", getLongVector)

println!("getLongVectorAsList")
if 'getLongVectorAsList' in locals():
	panic!("getLongVectorAsList already defined!")
getLongVectorAsList = callback("getLongVectorAsList")
getLongVectorAsListType = type(getLongVectorAsList[0])
println!(getLongVectorAsListType)
println!(getLongVectorAsList)
if(getLongVectorAsListType is not long):
	panic!("getLongVectorAsList not long!")
callback("setLongVectorAsList", getLongVectorAsList)

println!("getLongMatrix")
if 'getLongMatrix' in locals():
	panic!("getLongMatrix already defined!")
getLongMatrix = callback("getLongMatrix")
getLongMatrixType = type(getLongMatrix[0][0])
println!(getLongMatrixType)
println!(getLongMatrix)
if(getLongMatrixType is not long):
	panic!("getLongMatrix not long!")
callback("setLongMatrix", getLongMatrix)

println!("getLongMatrixAsList")
if 'getLongMatrixAsList' in locals():
	panic!("getLongMatrixAsList already defined!")
getLongMatrixAsList = callback("getLongMatrixAsList")
getLongMatrixAsListType = type(getLongMatrixAsList[0][0])
println!(getLongMatrixAsListType)
println!(getLongMatrixAsList)
if(getLongMatrixAsListType is not long):
	panic!("getLongMatrixAsList not long!")
callback("setLongMatrixAsList", getLongMatrixAsList)
