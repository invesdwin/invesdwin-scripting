print("getBoolean")
if(exists("getBoolean")){
	stop("getBoolean already defined!")
}
getBoolean <- callback("getBoolean")
print(typeof(getBoolean))
print(getBoolean)
if(typeof(getBoolean) != "logical"){
	stop("getBoolean not logical!")
}
callback("setBoolean",getBoolean)

print("getBooleanVector")
if(exists("getBooleanVector")){
	stop("getBooleanVector already defined!")
}
getBooleanVector <- callback("getBooleanVector")
print(typeof(getBooleanVector))
print(getBooleanVector)
if(typeof(getBooleanVector) != "logical"){
	stop("getBooleanVector not logical!")
}
callback("setBooleanVector",list(getBooleanVector))

print("getBooleanVectorAsList")
if(exists("getBooleanVectorAsList")){
	stop("getBooleanVectorAsList already defined!")
}
getBooleanVectorAsList <- callback("getBooleanVectorAsList")
print(typeof(getBooleanVectorAsList))
print(getBooleanVectorAsList)
if(typeof(getBooleanVectorAsList) != "logical"){
	stop("getBooleanVectorAsList not logical!")
}
callback("setBooleanVectorAsList",list(getBooleanVectorAsList))

print("getBooleanMatrix")
if(exists("getBooleanMatrix")){
	stop("getBooleanMatrix already defined!")
}
getBooleanMatrix <- callback("getBooleanMatrix")
print(typeof(getBooleanMatrix))
print(getBooleanMatrix)
if(typeof(getBooleanMatrix) != "logical"){
	stop("getBooleanMatrix not logical!")
}
callback("setBooleanMatrix",list(getBooleanMatrix))

print("getBooleanMatrixAsList")
if(exists("getBooleanMatrixAsList")){
	stop("getBooleanMatrixAsList already defined!")
}
getBooleanMatrixAsList <- callback("getBooleanMatrixAsList")
print(typeof(getBooleanMatrixAsList))
print(getBooleanMatrixAsList)
if(typeof(getBooleanMatrixAsList) != "logical"){
	stop("getBooleanMatrixAsList not logical!")
}
callback("setBooleanMatrixAsList",list(getBooleanMatrixAsList))
