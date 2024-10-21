print("getByte")
if(exists("getByte")){
	stop("getByte already defined!")
}
getByte <- callback("getByte")
print(typeof(getByte))
print(getByte)
if(typeof(getByte) != "integer"){
	stop("getByte not integer!")
}
callback("setByte",getByte)

print("getByteVector")
if(exists("getByteVector")){
	stop("getByteVector already defined!")
}
getByteVector <- callback("getByteVector")
print(typeof(getByteVector))
print(getByteVector)
if(typeof(getByteVector) != "integer"){
	stop("getByteVector not integer!")
}
callback("setByteVector",list(getByteVector))

print("getByteVectorAsList")
if(exists("getByteVectorAsList")){
	stop("getByteVectorAsList already defined!")
}
getByteVectorAsList <- callback("getByteVectorAsList")
print(typeof(getByteVectorAsList))
print(getByteVectorAsList)
if(typeof(getByteVectorAsList) != "integer"){
	stop("getByteVectorAsList not integer!")
}
callback("setByteVectorAsList",list(getByteVectorAsList))

print("getByteMatrix")
if(exists("getByteMatrix")){
	stop("getByteMatrix already defined!")
}
getByteMatrix <- callback("getByteMatrix")
print(typeof(getByteMatrix))
print(getByteMatrix)
if(typeof(getByteMatrix) != "integer"){
	stop("getByteMatrix not integer!")
}
callback("setByteMatrix",list(getByteMatrix))

print("getByteMatrixAsList")
if(exists("getByteMatrixAsList")){
	stop("getByteMatrixAsList already defined!")
}
getByteMatrixAsList <- callback("getByteMatrixAsList")
print(typeof(getByteMatrixAsList))
print(getByteMatrixAsList)
if(typeof(getByteMatrixAsList) != "integer"){
	stop("getByteMatrixAsList not integer!")
}
callback("setByteMatrixAsList",list(getByteMatrixAsList))
