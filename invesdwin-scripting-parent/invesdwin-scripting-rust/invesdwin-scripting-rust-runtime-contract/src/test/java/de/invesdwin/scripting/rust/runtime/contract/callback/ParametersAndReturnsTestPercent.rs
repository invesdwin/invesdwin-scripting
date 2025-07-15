println!("getPercent")
if 'getPercent' in locals():
	raise Exception("getPercent already defined!")
getPercent = callback("getPercent")
getPercentType = type(getPercent)
println!(getPercentType)
println!(getPercent)
if(getPercentType is not float):
	raise Exception("getPercent not float!")
callback("setPercent", getPercent)

println!("getPercentVector")
if 'getPercentVector' in locals():
	raise Exception("getPercentVector already defined!")
getPercentVector = callback("getPercentVector")
getPercentVectorType = type(getPercentVector[0])
println!(getPercentVectorType)
println!(getPercentVector)
if(getPercentVectorType is not float):
	raise Exception("getPercentVector not float!")
callback("setPercentVector", getPercentVector)

println!("getPercentVectorAsList")
if 'getPercentVectorAsList' in locals():
	raise Exception("getPercentVectorAsList already defined!")
getPercentVectorAsList = callback("getPercentVectorAsList")
getPercentVectorAsListType = type(getPercentVectorAsList[0])
println!(getPercentVectorAsListType)
println!(getPercentVectorAsList)
if(getPercentVectorAsListType is not float):
	raise Exception("getPercentVectorAsList not float!")
callback("setPercentVectorAsList", getPercentVectorAsList)

println!("getPercentMatrix")
if 'getPercentMatrix' in locals():
	raise Exception("getPercentMatrix already defined!")
getPercentMatrix = callback("getPercentMatrix")
getPercentMatrixType = type(getPercentMatrix[0][0])
println!(getPercentMatrixType)
println!(getPercentMatrix)
if(getPercentMatrixType is not float):
	raise Exception("getPercentMatrix not float!")
callback("setPercentMatrix", getPercentMatrix)

println!("getPercentMatrixAsList")
if 'getPercentMatrixAsList' in locals():
	raise Exception("getPercentMatrixAsList already defined!")
getPercentMatrixAsList = callback("getPercentMatrixAsList")
getPercentMatrixAsListType = type(getPercentMatrixAsList[0][0])
println!(getPercentMatrixAsListType)
println!(getPercentMatrixAsList)
if(getPercentMatrixAsListType is not float):
	raise Exception("getPercentMatrixAsList not float!")
callback("setPercentMatrixAsList", getPercentMatrixAsList)
