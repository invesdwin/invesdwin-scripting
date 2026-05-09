arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getString")
if(getString ~= nil) then
	error("getString already defined!")
end
getString = callback("getString")
getStringType = type(getString)
print(getStringType)
print(getString)
if(getStringType ~= "string") then
	error("getString not String!")
end
callback("setString", getString)

print("getStringWithNull")
if(getStringWithNull ~= nil) then
	error("getStringWithNull already defined!")
end
getStringWithNull = callback("getStringWithNull")
getStringWithNullType = type(getStringWithNull)
print(getStringWithNullType)
print(getStringWithNull)
if(getStringWithNull ~= nil) then
	error("getStringWithNull not null!")
end
callback("setStringWithNull", getStringWithNull)

print("getStringVector")
if(getStringVector ~= nil) then
	error("getStringVector already defined!")
end
getStringVector = callback("getStringVector")
getStringVectorType = type(getStringVector[1])
print(getStringVectorType)
print(arraysToString(getStringVector))
if(getStringVectorType ~= "string") then
	error("getStringVector not String!")
end
callback("setStringVector", getStringVector)

print("getStringVectorWithNull")
if(getStringVectorWithNull ~= nil) then
	error("getStringVectorWithNull already defined!")
end
getStringVectorWithNull = callback("getStringVectorWithNull")
getStringVectorWithNullType = type(getStringVectorWithNull[1])
print(getStringVectorWithNullType)
print(arraysToString(getStringVectorWithNull))
if(getStringVectorWithNullType ~= "string") then
	error("getStringVectorWithNull not String!")
end
if(getStringVectorWithNull[2] ~= nil) then
	error("getStringVectorWithNull[2] not null!")
end
callback("setStringVectorWithNull", getStringVectorWithNull)

print("getStringVectorAsList")
if(getStringVectorAsList ~= nil) then
	error("getStringVectorAsList already defined!")
end
getStringVectorAsList = callback("getStringVectorAsList")
getStringVectorAsListType = type(getStringVectorAsList[1])
print(getStringVectorAsListType)
print(arraysToString(getStringVectorAsList))
if(getStringVectorAsListType ~= "string") then
	error("getStringVectorAsList not String!")
end
callback("setStringVectorAsList", getStringVectorAsList)

print("getStringVectorAsListWithNull")
if(getStringVectorAsListWithNull ~= nil) then
	error("getStringVectorAsListWithNull already defined!")
end
getStringVectorAsListWithNull = callback("getStringVectorAsListWithNull")
getStringVectorAsListWithNullType = type(getStringVectorAsListWithNull[1])
print(getStringVectorAsListWithNullType)
print(arraysToString(getStringVectorAsListWithNull))
if(getStringVectorAsListWithNullType ~= "string") then
	error("getStringVectorAsListWithNull not String!")
end
if(getStringVectorAsListWithNull[2] ~= nil) then
	error("getStringVectorAsListWithNull[2] not null!")
end
callback("setStringVectorAsListWithNull", getStringVectorAsListWithNull)
print(getStringVectorAsListWithNullType)
print(getStringVectorAsListWithNull)
if(getStringVectorAsListWithNullType ~= "string") then
	error("getStringVectorAsListWithNull not String!")
end
if(getStringVectorAsListWithNull[2] ~= nil) then
	error("getStringVectorAsListWithNull[2] not null!")
end
callback("setStringVectorAsListWithNull", getStringVectorAsListWithNull)

print("getStringMatrix")
if(getStringMatrix ~= nil) then
	error("getStringMatrix already defined!")
end
getStringMatrix = callback("getStringMatrix")
getStringMatrixType = type(getStringMatrix[1][1])
print(getStringMatrixType)
print(arraysToString(getStringMatrix))
if(getStringMatrixType ~= "string") then
	error("getStringMatrix not String!")
end
callback("setStringMatrix", getStringMatrix)

print("getStringMatrixWithNull")
if(getStringMatrixWithNull ~= nil) then
	error("getStringMatrixWithNull already defined!")
end
getStringMatrixWithNull = callback("getStringMatrixWithNull")
getStringMatrixWithNullType = type(getStringMatrixWithNull[1][2])
print(getStringMatrixWithNullType)
print(arraysToString(getStringMatrixWithNull))
if(getStringMatrixWithNullType ~= "string") then
	error("getStringMatrixWithNull not String!")
end
if(getStringMatrixWithNull[1][1] ~= nil) then
	error("getStringMatrixWithNull[1][1] not null!")
end
if(getStringMatrixWithNull[2][2] ~= nil) then
	error("getStringMatrixWithNull[2][2] not null!")
end
if(getStringMatrixWithNull[3][3] ~= nil) then
	error("getStringMatrixWithNull[3][3] not null!")
end
callback("setStringMatrixWithNull", getStringMatrixWithNull)

print("getStringMatrixAsList")
if(getStringMatrixAsList ~= nil) then
	error("getStringMatrixAsList already defined!")
end
getStringMatrixAsList = callback("getStringMatrixAsList")
getStringMatrixAsListType = type(getStringMatrixAsList[1][1])
print(getStringMatrixAsListType)
print(arraysToString(getStringMatrixAsList))
if(getStringMatrixAsListType ~= "string") then
	error("getStringMatrixAsList not String!")
end
callback("setStringMatrixAsList", getStringMatrixAsList)

print("getStringMatrixAsListWithNull")
if(getStringMatrixAsListWithNull ~= nil) then
	error("getStringMatrixAsListWithNull already defined!")
end
getStringMatrixAsListWithNull = callback("getStringMatrixAsListWithNull")
getStringMatrixAsListWithNullType = type(getStringMatrixAsListWithNull[1][2])
print(getStringMatrixAsListWithNullType)
print(arraysToString(getStringMatrixAsListWithNull))
if(getStringMatrixAsListWithNullType ~= "string") then
	error("getStringMatrixAsListWithNull not String!")
end
if(getStringMatrixAsListWithNull[1][1] ~= nil) then
	error("getStringMatrixAsListWithNull[1][1] not null!")
end
if(getStringMatrixAsListWithNull[2][2] ~= nil) then
	error("getStringMatrixAsListWithNull[2][2] not null!")
end
if(getStringMatrixAsListWithNull[3][3] ~= nil) then
	error("getStringMatrixAsListWithNull[3][3] not null!")
end
callback("setStringMatrixAsListWithNull", getStringMatrixAsListWithNull)
