arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getCharacter")
if(getCharacter ~= nil) then
	error("getCharacter already defined!")
end
getCharacter = putCharacter
getCharacterType = type(getCharacter)
print(getCharacterType)
print(getCharacter)
if(getCharacterType ~= "string") then
	error("getCharacter not Character!")
end

print("getCharacterVector")
if(getCharacterVector ~= nil) then
	error("getCharacterVector already defined!")
end
getCharacterVector = putCharacterVector
getCharacterVectorType = type(getCharacterVector[1])
print(getCharacterVectorType)
print(arraysToString(getCharacterVector))
if(getCharacterVectorType ~= "string") then
	error("getCharacterVector not Character!")
end

print("getCharacterVectorAsList")
if(getCharacterVectorAsList ~= nil) then
	error("getCharacterVectorAsList already defined!")
end
getCharacterVectorAsList = putCharacterVectorAsList
getCharacterVectorAsListType = type(getCharacterVectorAsList[1])
print(getCharacterVectorAsListType)
print(arraysToString(getCharacterVectorAsList))
if(getCharacterVectorAsListType ~= "string") then
	error("getCharacterVectorAsList not Character!")
end

print("getCharacterMatrix")
if(getCharacterMatrix ~= nil) then
	error("getCharacterMatrix already defined!")
end
getCharacterMatrix = putCharacterMatrix
getCharacterMatrixType = type(getCharacterMatrix[1][1])
print(getCharacterMatrixType)
print(arraysToString(getCharacterMatrix))
if(getCharacterMatrixType ~= "string") then
	error("getCharacterMatrix not Character!")
end

print("getCharacterMatrixAsList")
if(getCharacterMatrixAsList ~= nil) then
	error("getCharacterMatrixAsList already defined!")
end
getCharacterMatrixAsList = putCharacterMatrixAsList
getCharacterMatrixAsListType = type(getCharacterMatrixAsList[1][1])
print(getCharacterMatrixAsListType)
print(arraysToString(getCharacterMatrixAsList))
if(getCharacterMatrixAsListType ~= "string") then
	error("getCharacterMatrixAsList not Character!")
end