print("getDecimal")
if(exists("getDecimal")){
	stop("getDecimal already defined!")
}
getDecimal <- putDecimal
print(typeof(getDecimal))
print(getDecimal)
if(typeof(getDecimal) != "double"){
	stop("getDecimal not double!")
}

print("getDecimalVector")
if(exists("getDecimalVector")){
	stop("getDecimalVector already defined!")
}
getDecimalVector <- putDecimalVector
print(typeof(getDecimalVector))
print(getDecimalVector)
if(typeof(getDecimalVector) != "double"){
	stop("getDecimalVector not double!")
}

print("getDecimalVectorAsList")
if(exists("getDecimalVectorAsList")){
	stop("getDecimalVectorAsList already defined!")
}
getDecimalVectorAsList <- putDecimalVectorAsList
print(typeof(getDecimalVectorAsList))
print(getDecimalVectorAsList)
if(typeof(getDecimalVectorAsList) != "double"){
	stop("getDecimalVectorAsList not double!")
}

print("getDecimalMatrix")
if(exists("getDecimalMatrix")){
	stop("getDecimalMatrix already defined!")
}
getDecimalMatrix <- putDecimalMatrix
print(typeof(getDecimalMatrix))
print(getDecimalMatrix)
if(typeof(getDecimalMatrix) != "double"){
	stop("getDecimalMatrix not double!")
}

print("getDecimalMatrixAsList")
if(exists("getDecimalMatrixAsList")){
	stop("getDecimalMatrixAsList already defined!")
}
getDecimalMatrixAsList <- putDecimalMatrixAsList
print(typeof(getDecimalMatrixAsList))
print(getDecimalMatrixAsList)
if(typeof(getDecimalMatrixAsList) != "double"){
	stop("getDecimalMatrixAsList not double!")
}
