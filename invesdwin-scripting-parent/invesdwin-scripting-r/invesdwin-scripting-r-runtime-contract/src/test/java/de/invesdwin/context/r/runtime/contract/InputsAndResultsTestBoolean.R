print("getBoolean")
if(exists("getBoolean")){
	stop("getBoolean already defined!")
}
getBoolean <- putBoolean
print(typeof(getBoolean))
print(getBoolean)
if(typeof(getBoolean) != "logical"){
	stop("getBoolean not logical!")
}

print("getBooleanVector")
if(exists("getBooleanVector")){
	stop("getBooleanVector already defined!")
}
getBooleanVector <- putBooleanVector
print(typeof(getBooleanVector))
print(getBooleanVector)
if(typeof(getBooleanVector) != "logical"){
	stop("getBooleanVector not logical!")
}

print("getBooleanVectorAsList")
if(exists("getBooleanVectorAsList")){
	stop("getBooleanVectorAsList already defined!")
}
getBooleanVectorAsList <- putBooleanVectorAsList
print(typeof(getBooleanVectorAsList))
print(getBooleanVectorAsList)
if(typeof(getBooleanVectorAsList) != "logical"){
	stop("getBooleanVectorAsList not logical!")
}

print("getBooleanMatrix")
if(exists("getBooleanMatrix")){
	stop("getBooleanMatrix already defined!")
}
getBooleanMatrix <- putBooleanMatrix
print(typeof(getBooleanMatrix))
print(getBooleanMatrix)
if(typeof(getBooleanMatrix) != "logical"){
	stop("getBooleanMatrix not logical!")
}

print("getBooleanMatrixAsList")
if(exists("getBooleanMatrixAsList")){
	stop("getBooleanMatrixAsList already defined!")
}
getBooleanMatrixAsList <- putBooleanMatrixAsList
print(typeof(getBooleanMatrixAsList))
print(getBooleanMatrixAsList)
if(typeof(getBooleanMatrixAsList) != "logical"){
	stop("getBooleanMatrixAsList not logical!")
}
