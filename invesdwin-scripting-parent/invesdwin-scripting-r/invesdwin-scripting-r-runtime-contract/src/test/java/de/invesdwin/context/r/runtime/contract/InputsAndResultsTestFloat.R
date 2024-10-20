print("getFloat")
if(exists("getFloat")){
	stop("getFloat already defined!")
}
getFloat <- putFloat
print(typeof(getFloat))
print(getFloat)
if(typeof(getFloat) != "double"){
	stop("getFloat not double!")
}

print("getFloatVector")
if(exists("getFloatVector")){
	stop("getFloatVector already defined!")
}
getFloatVector <- putFloatVector
print(typeof(getFloatVector))
print(getFloatVector)
if(typeof(getFloatVector) != "double"){
	stop("getFloatVector not double!")
}

print("getFloatVectorAsList")
if(exists("getFloatVectorAsList")){
	stop("getFloatVectorAsList already defined!")
}
getFloatVectorAsList <- putFloatVectorAsList
print(typeof(getFloatVectorAsList))
print(getFloatVectorAsList)
if(typeof(getFloatVectorAsList) != "double"){
	stop("getFloatVectorAsList not double!")
}

print("getFloatMatrix")
if(exists("getFloatMatrix")){
	stop("getFloatMatrix already defined!")
}
getFloatMatrix <- putFloatMatrix
print(typeof(getFloatMatrix))
print(getFloatMatrix)
if(typeof(getFloatMatrix) != "double"){
	stop("getFloatMatrix not double!")
}

print("getFloatMatrixAsList")
if(exists("getFloatMatrixAsList")){
	stop("getFloatMatrixAsList already defined!")
}
getFloatMatrixAsList <- putFloatMatrixAsList
print(typeof(getFloatMatrixAsList))
print(getFloatMatrixAsList)
if(typeof(getFloatMatrixAsList) != "double"){
	stop("getFloatMatrixAsList not double!")
}
