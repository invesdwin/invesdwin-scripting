print("getString")
if(exists("getString")){
	stop("getString already defined!")
}
getString <- callback("getString")
print(typeof(getString))
print(getString)
if(typeof(getString) != "character"){
	stop("getString not character!")
}
callback("setString",getString)

print("getStringWithNull")
if(exists("getStringWithNull")){
	stop("getStringWithNull already defined!")
}
getStringWithNull <- callback("getStringWithNull")
print(typeof(getStringWithNull))
print(getStringWithNull)
if(typeof(getStringWithNull) != "character"){
	stop("getStringWithNull not character!")
}
if(!is.na(getStringWithNull)){
	stop("getStringWithNull not na!")
}
callback("setStringWithNull",getStringWithNull)

print("getStringVector")
if(exists("getStringVector")){
	stop("getStringVector already defined!")
}
getStringVector <- callback("getStringVector")
print(typeof(getStringVector))
print(getStringVector)
if(typeof(getStringVector) != "character"){
	stop("getStringVector not character!")
}
callback("setStringVector",list(getStringVector))


print("getStringVectorWithNull")
if(exists("getStringVectorWithNull")){
	stop("getStringVectorWithNull already defined!")
}
getStringVectorWithNull <- callback("getStringVectorWithNull")
print(typeof(getStringVectorWithNull))
print(getStringVectorWithNull)
if(typeof(getStringVectorWithNull) != "character"){
	stop("getStringVectorWithNull not character!")
}
if(!is.na(getStringVectorWithNull[2])){
	stop("getStringVectorWithNull[2] not na!")
}
callback("setStringVectorWithNull",list(getStringVectorWithNull))

print("getStringVectorAsList")
if(exists("getStringVectorAsList")){
	stop("getStringVectorAsList already defined!")
}
getStringVectorAsList <- callback("getStringVectorAsList")
print(typeof(getStringVectorAsList))
print(getStringVectorAsList)
if(typeof(getStringVectorAsList) != "character"){
	stop("getStringVectorAsList not character!")
}
callback("setStringVectorAsList",list(getStringVectorAsList))

print("getStringVectorAsListWithNull")
if(exists("getStringVectorAsListWithNull")){
	stop("getStringVectorAsListWithNull already defined!")
}
getStringVectorAsListWithNull <- callback("getStringVectorAsListWithNull")
print(typeof(getStringVectorAsListWithNull))
print(getStringVectorAsListWithNull)
if(typeof(getStringVectorAsListWithNull) != "character"){
	stop("getStringVectorAsListWithNull not character!")
}
if(!is.na(getStringVectorAsListWithNull[2])){
	stop("getStringVectorAsListWithNull[2] not na!")
}
callback("setStringVectorAsListWithNull",list(getStringVectorAsListWithNull))

print("getStringMatrix")
if(exists("getStringMatrix")){
	stop("getStringMatrix already defined!")
}
getStringMatrix <- callback("getStringMatrix")
print(typeof(getStringMatrix))
print(getStringMatrix)
if(typeof(getStringMatrix) != "character"){
	stop("getStringMatrix not character!")
}
callback("setStringMatrix",list(getStringMatrix))


print("getStringMatrixWithNull")
if(exists("getStringMatrixWithNull")){
	stop("getStringMatrixWithNull already defined!")
}
getStringMatrixWithNull <- callback("getStringMatrixWithNull")
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
callback("setStringMatrixWithNull",list(getStringMatrixWithNull))

print("getStringMatrixAsList")
if(exists("getStringMatrixAsList")){
	stop("getStringMatrixAsList already defined!")
}
getStringMatrixAsList <- callback("getStringMatrixAsList")
print(typeof(getStringMatrixAsList))
print(getStringMatrixAsList)
if(typeof(getStringMatrixAsList) != "character"){
	stop("getStringMatrixAsList not character!")
}
callback("setStringMatrixAsList",list(getStringMatrixAsList))

print("getStringMatrixAsListWithNull")
if(exists("getStringMatrixAsListWithNull")){
	stop("getStringMatrixAsListWithNull already defined!")
}
getStringMatrixAsListWithNull <- callback("getStringMatrixAsListWithNull")
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
callback("setStringMatrixAsListWithNull",list(getStringMatrixAsListWithNull))
