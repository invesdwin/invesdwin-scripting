print("getFloat")
if(exists("getFloat")){
	stop("getFloat already defined!")
}
getFloat <- callback("getFloat")
print(typeof(getFloat))
print(getFloat)
if(typeof(getFloat) != "double"){
	stop("getFloat not double!")
}
callback("setFloat",getFloat)

print("getFloatVector")
if(exists("getFloatVector")){
	stop("getFloatVector already defined!")
}
getFloatVector <- callback("getFloatVector")
print(typeof(getFloatVector))
print(getFloatVector)
if(typeof(getFloatVector) != "double"){
	stop("getFloatVector not double!")
}
callback("setFloatVector",list(getFloatVector))

print("getFloatVectorAsList")
if(exists("getFloatVectorAsList")){
	stop("getFloatVectorAsList already defined!")
}
getFloatVectorAsList <- callback("getFloatVectorAsList")
print(typeof(getFloatVectorAsList))
print(getFloatVectorAsList)
if(typeof(getFloatVectorAsList) != "double"){
	stop("getFloatVectorAsList not double!")
}
callback("setFloatVectorAsList",list(getFloatVectorAsList))

print("getFloatMatrix")
if(exists("getFloatMatrix")){
	stop("getFloatMatrix already defined!")
}
getFloatMatrix <- callback("getFloatMatrix")
print(typeof(getFloatMatrix))
print(getFloatMatrix)
if(typeof(getFloatMatrix) != "double"){
	stop("getFloatMatrix not double!")
}
callback("setFloatMatrix",list(getFloatMatrix))

print("getFloatMatrixAsList")
if(exists("getFloatMatrixAsList")){
	stop("getFloatMatrixAsList already defined!")
}
getFloatMatrixAsList <- callback("getFloatMatrixAsList")
print(typeof(getFloatMatrixAsList))
print(getFloatMatrixAsList)
if(typeof(getFloatMatrixAsList) != "double"){
	stop("getFloatMatrixAsList not double!")
}
callback("setFloatMatrixAsList",list(getFloatMatrixAsList))
