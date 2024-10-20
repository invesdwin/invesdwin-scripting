print("getShort")
if(exists("getShort")){
	stop("getShort already defined!")
}
getShort <- callback("getShort")
print(typeof(getShort))
print(getShort)
if(typeof(getShort) != "integer"){
	stop("getShort not integer!")
}
callback("setShort",getShort)

print("getShortVector")
if(exists("getShortVector")){
	stop("getShortVector already defined!")
}
getShortVector <- callback("getShortVector")
print(typeof(getShortVector))
print(getShortVector)
if(typeof(getShortVector) != "integer"){
	stop("getShortVector not integer!")
}
callback("setShortVector",list(getShortVector))

print("getShortVectorAsList")
if(exists("getShortVectorAsList")){
	stop("getShortVectorAsList already defined!")
}
getShortVectorAsList <- callback("getShortVectorAsList")
print(typeof(getShortVectorAsList))
print(getShortVectorAsList)
if(typeof(getShortVectorAsList) != "integer"){
	stop("getShortVectorAsList not integer!")
}
callback("setShortVectorAsList",list(getShortVectorAsList))

print("getShortMatrix")
if(exists("getShortMatrix")){
	stop("getShortMatrix already defined!")
}
getShortMatrix <- callback("getShortMatrix")
print(typeof(getShortMatrix))
print(getShortMatrix)
if(typeof(getShortMatrix) != "integer"){
	stop("getShortMatrix not integer!")
}
callback("setShortMatrix",list(getShortMatrix))

print("getShortMatrixAsList")
if(exists("getShortMatrixAsList")){
	stop("getShortMatrixAsList already defined!")
}
getShortMatrixAsList <- callback("getShortMatrixAsList")
print(typeof(getShortMatrixAsList))
print(getShortMatrixAsList)
if(typeof(getShortMatrixAsList) != "integer"){
	stop("getShortMatrixAsList not integer!")
}
callback("setShortMatrixAsList",list(getShortMatrixAsList))
