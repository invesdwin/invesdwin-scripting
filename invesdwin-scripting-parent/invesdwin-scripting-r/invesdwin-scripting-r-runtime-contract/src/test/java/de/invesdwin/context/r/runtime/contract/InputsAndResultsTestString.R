print("getString")
if(exists("getString")){
	stop("getString already defined!")
}
getString <- putString
print(typeof(getString))
print(getString)
if(typeof(getString) != "character"){
	stop("getString not character!")
}

print("getStringWithNull")
if(exists("getStringWithNull")){
	stop("getStringWithNull already defined!")
}
getStringWithNull <- putStringWithNull
print(typeof(getStringWithNull))
print(getStringWithNull)
if(typeof(getStringWithNull) != "character"){
	stop("getStringWithNull not character!")
}
if(!is.na(getStringWithNull)){
	stop("getStringWithNull not na!")
}

print("getStringVector")
if(exists("getStringVector")){
	stop("getStringVector already defined!")
}
getStringVector <- putStringVector
print(typeof(getStringVector))
print(getStringVector)
if(typeof(getStringVector) != "character"){
	stop("getStringVector not character!")
}


print("getStringVectorWithNull")
if(exists("getStringVectorWithNull")){
	stop("getStringVectorWithNull already defined!")
}
getStringVectorWithNull <- putStringVectorWithNull
print(typeof(getStringVectorWithNull))
print(getStringVectorWithNull)
if(typeof(getStringVectorWithNull) != "character"){
	stop("getStringVectorWithNull not character!")
}
if(!is.na(getStringVectorWithNull[2])){
	stop("getStringVectorWithNull[2] not na!")
}

print("getStringVectorAsList")
if(exists("getStringVectorAsList")){
	stop("getStringVectorAsList already defined!")
}
getStringVectorAsList <- putStringVectorAsList
print(typeof(getStringVectorAsList))
print(getStringVectorAsList)
if(typeof(getStringVectorAsList) != "character"){
	stop("getStringVectorAsList not character!")
}

print("getStringVectorAsListWithNull")
if(exists("getStringVectorAsListWithNull")){
	stop("getStringVectorAsListWithNull already defined!")
}
getStringVectorAsListWithNull <- putStringVectorAsListWithNull
print(typeof(getStringVectorAsListWithNull))
print(getStringVectorAsListWithNull)
if(typeof(getStringVectorAsListWithNull) != "character"){
	stop("getStringVectorAsListWithNull not character!")
}
if(!is.na(getStringVectorAsListWithNull[2])){
	stop("getStringVectorAsListWithNull[2] not na!")
}

print("getStringMatrix")
if(exists("getStringMatrix")){
	stop("getStringMatrix already defined!")
}
getStringMatrix <- putStringMatrix
print(typeof(getStringMatrix))
print(getStringMatrix)
if(typeof(getStringMatrix) != "character"){
	stop("getStringMatrix not character!")
}


print("getStringMatrixWithNull")
if(exists("getStringMatrixWithNull")){
	stop("getStringMatrixWithNull already defined!")
}
getStringMatrixWithNull <- putStringMatrixWithNull
print(typeof(getStringMatrixWithNull))
print(getStringMatrixWithNull)
if(typeof(getStringMatrixWithNull) != "character"){
	stop("getStringMatrixWithNull not character!")
}
if(!is.na(getStringMatrixWithNull[1][1])){
	stop("getStringMatrixWithNull[1][1] not na!")
}
if(!is.na(getStringMatrixWithNull[2][2])){
	stop("getStringMatrixWithNull[2][2] not na!")
}
if(!is.na(getStringMatrixWithNull[3][3])){
	stop("getStringMatrixWithNull[3][3] not na!")
}

print("getStringMatrixAsList")
if(exists("getStringMatrixAsList")){
	stop("getStringMatrixAsList already defined!")
}
getStringMatrixAsList <- putStringMatrixAsList
print(typeof(getStringMatrixAsList))
print(getStringMatrixAsList)
if(typeof(getStringMatrixAsList) != "character"){
	stop("getStringMatrixAsList not character!")
}

print("getStringMatrixAsListWithNull")
if(exists("getStringMatrixAsListWithNull")){
	stop("getStringMatrixAsListWithNull already defined!")
}
getStringMatrixAsListWithNull <- putStringMatrixAsListWithNull
print(typeof(getStringMatrixAsListWithNull))
print(getStringMatrixAsListWithNull)
if(typeof(getStringMatrixAsListWithNull) != "character"){
	stop("getStringMatrixAsListWithNull not character!")
}
if(!is.na(getStringMatrixAsListWithNull[1][1])){
	stop("getStringMatrixAsListWithNull[1][1] not na!")
}
if(!is.na(getStringMatrixAsListWithNull[2][2])){
	stop("getStringMatrixAsListWithNull[2][2] not na!")
}
if(!is.na(getStringMatrixAsListWithNull[3][3])){
	stop("getStringMatrixAsListWithNull[3][3] not na!")
}
