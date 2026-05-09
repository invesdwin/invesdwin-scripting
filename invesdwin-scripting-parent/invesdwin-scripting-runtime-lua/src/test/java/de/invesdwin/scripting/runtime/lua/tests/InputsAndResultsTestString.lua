arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getString")
if(getString ~= nil) then
	error("getString already defined!")
end
getString = putString
getStringType = type(getString)
print(getStringType)
print(getString)
if(getStringType ~= "string") then
	error("getString not String!")
end

print("getStringWithNull")
if(getStringWithNull ~= nil) then
	error("getStringWithNull already defined!")
end
getStringWithNull = putStringWithNull
getStringWithNullType = type(getStringWithNull)
print(getStringWithNullType)
print(getStringWithNull)
if(getStringWithNull ~= nil) then
	error("getStringWithNull not null!")
end

print("getStringVector")
if(getStringVector ~= nil) then
	error("getStringVector already defined!")
end
getStringVector = putStringVector
getStringVectorType = type(getStringVector[1])
print(getStringVectorType)
print(arraysToString(getStringVector))
if(getStringVectorType ~= "string") then
	error("getStringVector not String!")
end

print("getStringVectorWithNull")
if(getStringVectorWithNull ~= nil) then
	error("getStringVectorWithNull already defined!")
end
getStringVectorWithNull = putStringVectorWithNull
getStringVectorWithNullType = type(getStringVectorWithNull[1])
print(getStringVectorWithNullType)
print(arraysToString(getStringVectorWithNull))
if(getStringVectorWithNullType ~= "string") then
	error("getStringVectorWithNull not String!")
end
if(getStringVectorWithNull[2] ~= nil) then
	error("getStringVectorWithNull[1] not null!")
end

print("getStringVectorAsList")
if(getStringVectorAsList ~= nil) then
	error("getStringVectorAsList already defined!")
end
getStringVectorAsList = putStringVectorAsList
getStringVectorAsListType = type(getStringVectorAsList[1])
print(getStringVectorAsListType)
print(arraysToString(getStringVectorAsList))
if(getStringVectorAsListType ~= "string") then
	error("getStringVectorAsList not String!")
end

print("getStringVectorAsListWithNull")
if(getStringVectorAsListWithNull ~= nil) then
	error("getStringVectorAsListWithNull already defined!")
end
getStringVectorAsListWithNull = putStringVectorAsListWithNull
getStringVectorAsListWithNullType = type(getStringVectorAsListWithNull[1])
print(getStringVectorAsListWithNullType)
print(arraysToString(getStringVectorAsListWithNull))
if(getStringVectorAsListWithNullType ~= "string") then
	error("getStringVectorAsListWithNull not String!")
end
if(getStringVectorAsListWithNull[2] ~= nil) then
	error("getStringVectorAsListWithNull[1] not null!")
end

print("getStringMatrix")
if(getStringMatrix ~= nil) then
	error("getStringMatrix already defined!")
end
getStringMatrix = putStringMatrix
getStringMatrixType = type(getStringMatrix[1][1])
print(getStringMatrixType)
print(arraysToString(getStringMatrix))
if(getStringMatrixType ~= "string") then
	error("getStringMatrix not String!")
end

print("getStringMatrixWithNull")
if(getStringMatrixWithNull ~= nil) then
	error("getStringMatrixWithNull already defined!")
end
getStringMatrixWithNull = putStringMatrixWithNull
getStringMatrixWithNullType = type(getStringMatrixWithNull[1][2])
print(getStringMatrixWithNullType)
print(arraysToString(getStringMatrixWithNull))
if(getStringMatrixWithNullType ~= "string") then
	error("getStringMatrixWithNull not String!")
end
if(getStringMatrixWithNull[1][1] ~= nil) then
	error("getStringMatrixWithNull[0][0] not null!")
end
if(getStringMatrixWithNull[2][2] ~= nil) then
	error("getStringMatrixWithNull[1][1] not null!")
end
if(getStringMatrixWithNull[3][3] ~= nil) then
	error("getStringMatrixWithNull[2][2] not null!")
end

print("getStringMatrixAsList")
if(getStringMatrixAsList ~= nil) then
	error("getStringMatrixAsList already defined!")
end
getStringMatrixAsList = putStringMatrixAsList
getStringMatrixAsListType = type(getStringMatrixAsList[1][1])
print(getStringMatrixAsListType)
print(arraysToString(getStringMatrixAsList))
if(getStringMatrixAsListType ~= "string") then
	error("getStringMatrixAsList not String!")
end

print("getStringMatrixAsListWithNull")
if(getStringMatrixAsListWithNull ~= nil) then
	error("getStringMatrixAsListWithNull already defined!")
end
getStringMatrixAsListWithNull = putStringMatrixAsListWithNull
getStringMatrixAsListWithNullType = type(getStringMatrixAsListWithNull[1][2])
print(getStringMatrixAsListWithNullType)
print(arraysToString(getStringMatrixAsListWithNull))
if(getStringMatrixAsListWithNullType ~= "string") then
	error("getStringMatrixAsListWithNull not String!")
end
if(getStringMatrixAsListWithNull[1][1] ~= nil) then
	error("getStringMatrixAsListWithNull[0][0] not null!")
end
if(getStringMatrixAsListWithNull[2][2] ~= nil) then
	error("getStringMatrixAsListWithNull[1][1] not null!")
end
if(getStringMatrixAsListWithNull[3][3] ~= nil) then
	error("getStringMatrixAsListWithNull[2][2] not null!")
end
