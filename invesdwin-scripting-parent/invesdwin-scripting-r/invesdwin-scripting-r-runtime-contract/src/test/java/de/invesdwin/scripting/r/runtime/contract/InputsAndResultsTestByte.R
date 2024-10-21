print("getByte")
if(exists("getByte")){
	stop("getByte already defined!")
}
getByte <- putByte
print(typeof(getByte))
print(getByte)
if(typeof(getByte) != "integer"){
	stop("getByte not integer!")
}

print("getByteVector")
if(exists("getByteVector")){
	stop("getByteVector already defined!")
}
getByteVector <- putByteVector
print(typeof(getByteVector))
print(getByteVector)
if(typeof(getByteVector) != "integer"){
	stop("getByteVector not integer!")
}

print("getByteVectorAsList")
if(exists("getByteVectorAsList")){
	stop("getByteVectorAsList already defined!")
}
getByteVectorAsList <- putByteVectorAsList
print(typeof(getByteVectorAsList))
print(getByteVectorAsList)
if(typeof(getByteVectorAsList) != "integer"){
	stop("getByteVectorAsList not integer!")
}

print("getByteMatrix")
if(exists("getByteMatrix")){
	stop("getByteMatrix already defined!")
}
getByteMatrix <- putByteMatrix
print(typeof(getByteMatrix))
print(getByteMatrix)
if(typeof(getByteMatrix) != "integer"){
	stop("getByteMatrix not integer!")
}

print("getByteMatrixAsList")
if(exists("getByteMatrixAsList")){
	stop("getByteMatrixAsList already defined!")
}
getByteMatrixAsList <- putByteMatrixAsList
print(typeof(getByteMatrixAsList))
print(getByteMatrixAsList)
if(typeof(getByteMatrixAsList) != "integer"){
	stop("getByteMatrixAsList not integer!")
}
