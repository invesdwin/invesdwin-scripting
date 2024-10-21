print("getDecimal")
if(exists("getDecimal")){
	stop("getDecimal already defined!")
}
getDecimal <- callback("getDecimal")
print(typeof(getDecimal))
print(getDecimal)
if(typeof(getDecimal) != "double"){
	stop("getDecimal not double!")
}
callback("setDecimal",getDecimal)

print("getDecimalVector")
if(exists("getDecimalVector")){
	stop("getDecimalVector already defined!")
}
getDecimalVector <- callback("getDecimalVector")
print(typeof(getDecimalVector))
print(getDecimalVector)
if(typeof(getDecimalVector) != "double"){
	stop("getDecimalVector not double!")
}
callback("setDecimalVector",list(getDecimalVector))

print("getDecimalVectorAsList")
if(exists("getDecimalVectorAsList")){
	stop("getDecimalVectorAsList already defined!")
}
getDecimalVectorAsList <- callback("getDecimalVectorAsList")
print(typeof(getDecimalVectorAsList))
print(getDecimalVectorAsList)
if(typeof(getDecimalVectorAsList) != "double"){
	stop("getDecimalVectorAsList not double!")
}
callback("setDecimalVectorAsList",list(getDecimalVectorAsList))

print("getDecimalMatrix")
if(exists("getDecimalMatrix")){
	stop("getDecimalMatrix already defined!")
}
getDecimalMatrix <- callback("getDecimalMatrix")
print(typeof(getDecimalMatrix))
print(getDecimalMatrix)
if(typeof(getDecimalMatrix) != "double"){
	stop("getDecimalMatrix not double!")
}
callback("setDecimalMatrix",list(getDecimalMatrix))

print("getDecimalMatrixAsList")
if(exists("getDecimalMatrixAsList")){
	stop("getDecimalMatrixAsList already defined!")
}
getDecimalMatrixAsList <- callback("getDecimalMatrixAsList")
print(typeof(getDecimalMatrixAsList))
print(getDecimalMatrixAsList)
if(typeof(getDecimalMatrixAsList) != "double"){
	stop("getDecimalMatrixAsList not double!")
}
callback("setDecimalMatrixAsList",list(getDecimalMatrixAsList))
