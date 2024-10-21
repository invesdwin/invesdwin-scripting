print("getCharacter")
if(exists("getCharacter")){
	stop("getCharacter already defined!")
}
getCharacter <- putCharacter
print(typeof(getCharacter))
print(getCharacter)
if(typeof(getCharacter) != "character"){
	stop("getCharacter not character!")
}

print("getCharacterVector")
if(exists("getCharacterVector")){
	stop("getCharacterVector already defined!")
}
getCharacterVector <- putCharacterVector
print(typeof(getCharacterVector))
print(getCharacterVector)
if(typeof(getCharacterVector) != "character"){
	stop("getCharacterVector not character!")
}

print("getCharacterVectorAsList")
if(exists("getCharacterVectorAsList")){
	stop("getCharacterVectorAsList already defined!")
}
getCharacterVectorAsList <- putCharacterVectorAsList
print(typeof(getCharacterVectorAsList))
print(getCharacterVectorAsList)
if(typeof(getCharacterVectorAsList) != "character"){
	stop("getCharacterVectorAsList not character!")
}

print("getCharacterMatrix")
if(exists("getCharacterMatrix")){
	stop("getCharacterMatrix already defined!")
}
getCharacterMatrix <- putCharacterMatrix
print(typeof(getCharacterMatrix))
print(getCharacterMatrix)
if(typeof(getCharacterMatrix) != "character"){
	stop("getCharacterMatrix not character!")
}

print("getCharacterMatrixAsList")
if(exists("getCharacterMatrixAsList")){
	stop("getCharacterMatrixAsList already defined!")
}
getCharacterMatrixAsList <- putCharacterMatrixAsList
print(typeof(getCharacterMatrixAsList))
print(getCharacterMatrixAsList)
if(typeof(getCharacterMatrixAsList) != "character"){
	stop("getCharacterMatrixAsList not character!")
}