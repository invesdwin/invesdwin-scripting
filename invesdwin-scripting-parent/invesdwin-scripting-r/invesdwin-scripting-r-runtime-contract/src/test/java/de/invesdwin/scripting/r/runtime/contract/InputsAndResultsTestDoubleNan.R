print("getDouble")
if(exists("getDouble")){
	stop("getDouble already defined!")
}
getDouble <- putDouble
print(typeof(getDouble))
print(getDouble)
if(typeof(getDouble) != "double"){
	stop("getDouble not double!")
}
if(!is.nan(getDouble)){
	stop("getDouble not NaN!")
}

print("getDoubleVector")
if(exists("getDoubleVector")){
	stop("getDoubleVector already defined!")
}
getDoubleVector <- putDoubleVector
print(typeof(getDoubleVector))
print(getDoubleVector)
if(typeof(getDoubleVector) != "double"){
	stop("getDoubleVector not double!")
}
if(!is.nan(getDoubleVector[2])){
	stop("getDoubleVector[2] not NaN!")
}

print("getDoubleVectorAsList")
if(exists("getDoubleVectorAsList")){
	stop("getDoubleVectorAsList already defined!")
}
getDoubleVectorAsList <- putDoubleVectorAsList
print(typeof(getDoubleVectorAsList))
print(getDoubleVectorAsList)
if(typeof(getDoubleVectorAsList) != "double"){
	stop("getDoubleVectorAsList not double!")
}
if(!is.nan(getDoubleVectorAsList[2])){
	stop("getDoubleVectorAsList[2] not NaN!")
}

print("getDoubleMatrix")
if(exists("getDoubleMatrix")){
	stop("getDoubleMatrix already defined!")
}
getDoubleMatrix <- putDoubleMatrix
print(typeof(getDoubleMatrix))
print(getDoubleMatrix)
if(typeof(getDoubleMatrix) != "double"){
	stop("getDoubleMatrix not double!")
}
if(!is.nan(getDoubleMatrix[1][1])){
	stop("getDoubleMatrix[1][1] not NaN!")
}
# somehow R turns NaN to na on get after first matrix row, even though print shows NaN
# is.na checks both na and NaN anyhow and should be used instead anywhere
# we keep is.nan in the test to highlight this difference
if(!is.na(getDoubleMatrix[2][2])){
	stop("getDoubleMatrix[2][2] not NaN!")
}
if(!is.na(getDoubleMatrix[3][3])){
	stop("getDoubleMatrix[3][3] not NaN!")
}

print("getDoubleMatrixAsList")
if(exists("getDoubleMatrixAsList")){
	stop("getDoubleMatrixAsList already defined!")
}
getDoubleMatrixAsList <- putDoubleMatrixAsList
print(typeof(getDoubleMatrixAsList))
print(getDoubleMatrixAsList)
if(typeof(getDoubleMatrixAsList) != "double"){
	stop("getDoubleMatrixAsList not double!")
}
if(!is.nan(getDoubleMatrixAsList[1][1])){
	stop("getDoubleMatrixAsList[1][1] not NaN!")
}
if(!is.na(getDoubleMatrixAsList[2][2])){
	stop("getDoubleMatrixAsList[2][2] not NaN!")
}
if(!is.na(getDoubleMatrixAsList[3][3])){
	stop("getDoubleMatrixAsList[3][3] not NaN!")
}
