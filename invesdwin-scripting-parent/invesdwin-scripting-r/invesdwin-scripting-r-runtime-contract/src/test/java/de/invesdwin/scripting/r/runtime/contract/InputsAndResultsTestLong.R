print("getLong")
if(exists("getLong")){
	stop("getLong already defined!")
}
getLong <- putLong
print(typeof(getLong))
print(getLong)
if(typeof(getLong) != "double"){
	stop("getLong not double!")
}

print("getLongVector")
if(exists("getLongVector")){
	stop("getLongVector already defined!")
}
getLongVector <- putLongVector
print(typeof(getLongVector))
print(getLongVector)
if(typeof(getLongVector) != "double"){
	stop("getLongVector not double!")
}

print("getLongVectorAsList")
if(exists("getLongVectorAsList")){
	stop("getLongVectorAsList already defined!")
}
getLongVectorAsList <- putLongVectorAsList
print(typeof(getLongVectorAsList))
print(getLongVectorAsList)
if(typeof(getLongVectorAsList) != "double"){
	stop("getLongVectorAsList not double!")
}

print("getLongMatrix")
if(exists("getLongMatrix")){
	stop("getLongMatrix already defined!")
}
getLongMatrix <- putLongMatrix
print(typeof(getLongMatrix))
print(getLongMatrix)
if(typeof(getLongMatrix) != "double"){
	stop("getLongMatrix not double!")
}

print("getLongMatrixAsList")
if(exists("getLongMatrixAsList")){
	stop("getLongMatrixAsList already defined!")
}
getLongMatrixAsList <- putLongMatrixAsList
print(typeof(getLongMatrixAsList))
print(getLongMatrixAsList)
if(typeof(getLongMatrixAsList) != "double"){
	stop("getLongMatrixAsList not double!")
}
