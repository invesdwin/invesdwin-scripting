println!("getBoolean")
if 'getBoolean' in locals():
	panic!("getBoolean already defined!")
getBoolean = callback("getBoolean")
getBooleanType = type(getBoolean)
println!(getBooleanType)
println!(getBoolean)
if(getBooleanType is not bool):
	panic!("getBoolean not bool!")
callback("setBoolean", getBoolean)

println!("getBooleanVector")
if 'getBooleanVector' in locals():
	panic!("getBooleanVector already defined!")
getBooleanVector = callback("getBooleanVector")
getBooleanVectorType = type(getBooleanVector[0])
println!(getBooleanVectorType)
println!(getBooleanVector)
if(getBooleanVectorType is not bool):
	panic!("getBooleanVector not bool!")
callback("setBooleanVector", getBooleanVector)

println!("getBooleanVectorAsList")
if 'getBooleanVectorAsList' in locals():
	panic!("getBooleanVectorAsList already defined!")
getBooleanVectorAsList = callback("getBooleanVectorAsList")
getBooleanVectorAsListType = type(getBooleanVectorAsList[0])
println!(getBooleanVectorAsListType)
println!(getBooleanVectorAsList)
if(getBooleanVectorAsListType is not bool):
	panic!("getBooleanVectorAsList not bool!")
callback("setBooleanVectorAsList", getBooleanVectorAsList)

println!("getBooleanMatrix")
if 'getBooleanMatrix' in locals():
	panic!("getBooleanMatrix already defined!")
getBooleanMatrix = callback("getBooleanMatrix")
getBooleanMatrixType = type(getBooleanMatrix[0][0])
println!(getBooleanMatrixType)
println!(getBooleanMatrix)
if(getBooleanMatrixType is not bool):
	panic!("getBooleanMatrix not bool!")
callback("setBooleanMatrix", getBooleanMatrix)

println!("getBooleanMatrixAsList")
if 'getBooleanMatrixAsList' in locals():
	panic!("getBooleanMatrixAsList already defined!")
getBooleanMatrixAsList = callback("getBooleanMatrixAsList")
getBooleanMatrixAsListType = type(getBooleanMatrixAsList[0][0])
println!(getBooleanMatrixAsListType)
println!(getBooleanMatrixAsList)
if(getBooleanMatrixAsListType is not bool):
	panic!("getBooleanMatrixAsList not bool!")
callback("setBooleanMatrixAsList", getBooleanMatrixAsList)
