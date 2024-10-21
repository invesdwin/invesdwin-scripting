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
