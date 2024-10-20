print("getInteger")
if(exists("getInteger")){
	stop("getInteger already defined!")
}
getInteger <- callback("getInteger")
print(typeof(getInteger))
print(getInteger)
if(typeof(getInteger) != "integer"){
	stop("getInteger not integer!")
}
callback("setInteger",getInteger)

print("getIntegerVector")
if(exists("getIntegerVector")){
	stop("getIntegerVector already defined!")
}
getIntegerVector <- callback("getIntegerVector")
print(typeof(getIntegerVector))
print(getIntegerVector)
if(typeof(getIntegerVector) != "integer"){
	stop("getIntegerVector not integer!")
}
callback("setIntegerVector",list(getIntegerVector))

print("getIntegerVectorAsList")
if(exists("getIntegerVectorAsList")){
	stop("getIntegerVectorAsList already defined!")
}
getIntegerVectorAsList <- callback("getIntegerVectorAsList")
print(typeof(getIntegerVectorAsList))
print(getIntegerVectorAsList)
if(typeof(getIntegerVectorAsList) != "integer"){
	stop("getIntegerVectorAsList not integer!")
}
callback("setIntegerVectorAsList",list(getIntegerVectorAsList))

print("getIntegerMatrix")
if(exists("getIntegerMatrix")){
	stop("getIntegerMatrix already defined!")
}
getIntegerMatrix <- callback("getIntegerMatrix")
print(typeof(getIntegerMatrix))
print(getIntegerMatrix)
if(typeof(getIntegerMatrix) != "integer"){
	stop("getIntegerMatrix not integer!")
}
callback("setIntegerMatrix",list(getIntegerMatrix))

print("getIntegerMatrixAsList")
if(exists("getIntegerMatrixAsList")){
	stop("getIntegerMatrixAsList already defined!")
}
getIntegerMatrixAsList <- callback("getIntegerMatrixAsList")
print(typeof(getIntegerMatrixAsList))
print(getIntegerMatrixAsList)
if(typeof(getIntegerMatrixAsList) != "integer"){
	stop("getIntegerMatrixAsList not integer!")
}
callback("setIntegerMatrixAsList",list(getIntegerMatrixAsList))
