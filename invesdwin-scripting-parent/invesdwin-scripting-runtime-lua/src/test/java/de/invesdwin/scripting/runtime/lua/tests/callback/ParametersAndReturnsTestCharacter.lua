arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getCharacter")
if(getCharacter ~= nil) then
	error("getCharacter already defined!")
end
getCharacter = callback("getCharacter")
getCharacterType = type(getCharacter)
print(getCharacterType)
print(getCharacter)
if(getCharacterType ~= "string") then
	error("getCharacter not Character!")
end
callback("setCharacter", getCharacter)

print("getCharacterVector")
if(getCharacterVector ~= nil) then
	error("getCharacterVector already defined!")
end
getCharacterVector = callback("getCharacterVector")
getCharacterVectorType = type(getCharacterVector[1])
print(getCharacterVectorType)
print(arraysToString(getCharacterVector))
if(getCharacterVectorType ~= "string") then
	error("getCharacterVector not Character!")
end
callback("setCharacterVector", getCharacterVector)

print("getCharacterVectorAsList")
if(getCharacterVectorAsList ~= nil) then
	error("getCharacterVectorAsList already defined!")
end
getCharacterVectorAsList = callback("getCharacterVectorAsList")
getCharacterVectorAsListType = type(getCharacterVectorAsList[1])
print(getCharacterVectorAsListType)
print(arraysToString(getCharacterVectorAsList))
if(getCharacterVectorAsListType ~= "string") then
	error("getCharacterVectorAsList not Character!")
end
callback("setCharacterVectorAsList", getCharacterVectorAsList)

print("getCharacterMatrix")
if(getCharacterMatrix ~= nil) then
	error("getCharacterMatrix already defined!")
end
getCharacterMatrix = callback("getCharacterMatrix")
getCharacterMatrixType = type(getCharacterMatrix[1][1])
print(getCharacterMatrixType)
print(arraysToString(getCharacterMatrix))
if(getCharacterMatrixType ~= "string") then
	error("getCharacterMatrix not Character!")
end
callback("setCharacterMatrix", getCharacterMatrix)

print("getCharacterMatrixAsList")
if(getCharacterMatrixAsList ~= nil) then
	error("getCharacterMatrixAsList already defined!")
end
getCharacterMatrixAsList = callback("getCharacterMatrixAsList")
getCharacterMatrixAsListType = type(getCharacterMatrixAsList[1][1])
print(getCharacterMatrixAsListType)
print(arraysToString(getCharacterMatrixAsList))
if(getCharacterMatrixAsListType ~= "string") then
	error("getCharacterMatrixAsList not Character!")
end
callback("setCharacterMatrixAsList", getCharacterMatrixAsList)