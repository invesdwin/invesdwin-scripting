print("getLong")
if(exists("getLong")){
	stop("getLong already defined!")
}
getLong <- callback("getLong")
print(typeof(getLong))
print(getLong)
if(typeof(getLong) != "double"){
	stop("getLong not double!")
}
callback("setLong",getLong)

print("getLongVector")
if(exists("getLongVector")){
	stop("getLongVector already defined!")
}
getLongVector <- callback("getLongVector")
print(typeof(getLongVector))
print(getLongVector)
if(typeof(getLongVector) != "double"){
	stop("getLongVector not double!")
}
callback("setLongVector",list(getLongVector))

print("getLongVectorAsList")
if(exists("getLongVectorAsList")){
	stop("getLongVectorAsList already defined!")
}
getLongVectorAsList <- callback("getLongVectorAsList")
print(typeof(getLongVectorAsList))
print(getLongVectorAsList)
if(typeof(getLongVectorAsList) != "double"){
	stop("getLongVectorAsList not double!")
}
callback("setLongVectorAsList",list(getLongVectorAsList))

print("getLongMatrix")
if(exists("getLongMatrix")){
	stop("getLongMatrix already defined!")
}
getLongMatrix <- callback("getLongMatrix")
print(typeof(getLongMatrix))
print(getLongMatrix)
if(typeof(getLongMatrix) != "double"){
	stop("getLongMatrix not double!")
}
callback("setLongMatrix",list(getLongMatrix))

print("getLongMatrixAsList")
if(exists("getLongMatrixAsList")){
	stop("getLongMatrixAsList already defined!")
}
getLongMatrixAsList <- callback("getLongMatrixAsList")
print(typeof(getLongMatrixAsList))
print(getLongMatrixAsList)
if(typeof(getLongMatrixAsList) != "double"){
	stop("getLongMatrixAsList not double!")
}
callback("setLongMatrixAsList",list(getLongMatrixAsList))
