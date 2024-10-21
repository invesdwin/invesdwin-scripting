print("getPercent")
if(exists("getPercent")){
	stop("getPercent already defined!")
}
getPercent <- putPercent
print(typeof(getPercent))
print(getPercent)
if(typeof(getPercent) != "double"){
	stop("getPercent not double!")
}

print("getPercentVector")
if(exists("getPercentVector")){
	stop("getPercentVector already defined!")
}
getPercentVector <- putPercentVector
print(typeof(getPercentVector))
print(getPercentVector)
if(typeof(getPercentVector) != "double"){
	stop("getPercentVector not double!")
}

print("getPercentVectorAsList")
if(exists("getPercentVectorAsList")){
	stop("getPercentVectorAsList already defined!")
}
getPercentVectorAsList <- putPercentVectorAsList
print(typeof(getPercentVectorAsList))
print(getPercentVectorAsList)
if(typeof(getPercentVectorAsList) != "double"){
	stop("getPercentVectorAsList not double!")
}

print("getPercentMatrix")
if(exists("getPercentMatrix")){
	stop("getPercentMatrix already defined!")
}
getPercentMatrix <- putPercentMatrix
print(typeof(getPercentMatrix))
print(getPercentMatrix)
if(typeof(getPercentMatrix) != "double"){
	stop("getPercentMatrix not double!")
}

print("getPercentMatrixAsList")
if(exists("getPercentMatrixAsList")){
	stop("getPercentMatrixAsList already defined!")
}
getPercentMatrixAsList <- putPercentMatrixAsList
print(typeof(getPercentMatrixAsList))
print(getPercentMatrixAsList)
if(typeof(getPercentMatrixAsList) != "double"){
	stop("getPercentMatrixAsList not double!")
}
