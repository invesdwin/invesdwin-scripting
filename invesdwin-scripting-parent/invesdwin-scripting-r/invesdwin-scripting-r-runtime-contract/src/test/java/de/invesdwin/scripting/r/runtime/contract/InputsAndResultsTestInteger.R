print("getInteger")
if(exists("getInteger")){
	stop("getInteger already defined!")
}
getInteger <- putInteger
print(typeof(getInteger))
print(getInteger)
if(typeof(getInteger) != "integer"){
	stop("getInteger not integer!")
}

print("getIntegerVector")
if(exists("getIntegerVector")){
	stop("getIntegerVector already defined!")
}
getIntegerVector <- putIntegerVector
print(typeof(getIntegerVector))
print(getIntegerVector)
if(typeof(getIntegerVector) != "integer"){
	stop("getIntegerVector not integer!")
}

print("getIntegerVectorAsList")
if(exists("getIntegerVectorAsList")){
	stop("getIntegerVectorAsList already defined!")
}
getIntegerVectorAsList <- putIntegerVectorAsList
print(typeof(getIntegerVectorAsList))
print(getIntegerVectorAsList)
if(typeof(getIntegerVectorAsList) != "integer"){
	stop("getIntegerVectorAsList not integer!")
}

print("getIntegerMatrix")
if(exists("getIntegerMatrix")){
	stop("getIntegerMatrix already defined!")
}
getIntegerMatrix <- putIntegerMatrix
print(typeof(getIntegerMatrix))
print(getIntegerMatrix)
if(typeof(getIntegerMatrix) != "integer"){
	stop("getIntegerMatrix not integer!")
}

print("getIntegerMatrixAsList")
if(exists("getIntegerMatrixAsList")){
	stop("getIntegerMatrixAsList already defined!")
}
getIntegerMatrixAsList <- putIntegerMatrixAsList
print(typeof(getIntegerMatrixAsList))
print(getIntegerMatrixAsList)
if(typeof(getIntegerMatrixAsList) != "integer"){
	stop("getIntegerMatrixAsList not integer!")
}
