print("getShort")
if(exists("getShort")){
	stop("getShort already defined!")
}
getShort <- putShort
print(typeof(getShort))
print(getShort)
if(typeof(getShort) != "integer"){
	stop("getShort not integer!")
}

print("getShortVector")
if(exists("getShortVector")){
	stop("getShortVector already defined!")
}
getShortVector <- putShortVector
print(typeof(getShortVector))
print(getShortVector)
if(typeof(getShortVector) != "integer"){
	stop("getShortVector not integer!")
}

print("getShortVectorAsList")
if(exists("getShortVectorAsList")){
	stop("getShortVectorAsList already defined!")
}
getShortVectorAsList <- putShortVectorAsList
print(typeof(getShortVectorAsList))
print(getShortVectorAsList)
if(typeof(getShortVectorAsList) != "integer"){
	stop("getShortVectorAsList not integer!")
}

print("getShortMatrix")
if(exists("getShortMatrix")){
	stop("getShortMatrix already defined!")
}
getShortMatrix <- putShortMatrix
print(typeof(getShortMatrix))
print(getShortMatrix)
if(typeof(getShortMatrix) != "integer"){
	stop("getShortMatrix not integer!")
}

print("getShortMatrixAsList")
if(exists("getShortMatrixAsList")){
	stop("getShortMatrixAsList already defined!")
}
getShortMatrixAsList <- putShortMatrixAsList
print(typeof(getShortMatrixAsList))
print(getShortMatrixAsList)
if(typeof(getShortMatrixAsList) != "integer"){
	stop("getShortMatrixAsList not integer!")
}
