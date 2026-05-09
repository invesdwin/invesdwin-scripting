arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getFloat")
if(getFloat ~= nil) then
	error("getFloat already defined!")
end
getFloat = callback("getFloat")
getFloatType = type(getFloat)
print(getFloatType)
print(getFloat)
if(getFloatType ~= "number") then
	error("getFloat not Float!")
end
callback("setFloat", getFloat)

print("getFloatVector")
if(getFloatVector ~= nil) then
	error("getFloatVector already defined!")
end
getFloatVector = callback("getFloatVector")
getFloatVectorType = type(getFloatVector[1])
print(getFloatVectorType)
print(arraysToString(getFloatVector))
if(getFloatVectorType ~= "number") then
	error("getFloatVector not Float!")
end
callback("setFloatVector", getFloatVector)

print("getFloatVectorAsList")
if(getFloatVectorAsList ~= nil) then
	error("getFloatVectorAsList already defined!")
end
getFloatVectorAsList = callback("getFloatVectorAsList")
getFloatVectorAsListType = type(getFloatVectorAsList[1])
print(getFloatVectorAsListType)
print(arraysToString(getFloatVectorAsList))
if(getFloatVectorAsListType ~= "number") then
	error("getFloatVectorAsList not Float!")
end
callback("setFloatVectorAsList", getFloatVectorAsList)

print("getFloatMatrix")
if(getFloatMatrix ~= nil) then
	error("getFloatMatrix already defined!")
end
getFloatMatrix = callback("getFloatMatrix")
getFloatMatrixType = type(getFloatMatrix[1][1])
print(getFloatMatrixType)
print(arraysToString(getFloatMatrix))
if(getFloatMatrixType ~= "number") then
	error("getFloatMatrix not Float!")
end
callback("setFloatMatrix", getFloatMatrix)

print("getFloatMatrixAsList")
if(getFloatMatrixAsList ~= nil) then
	error("getFloatMatrixAsList already defined!")
end
getFloatMatrixAsList = callback("getFloatMatrixAsList")
getFloatMatrixAsListType = type(getFloatMatrixAsList[1][1])
print(getFloatMatrixAsListType)
print(arraysToString(getFloatMatrixAsList))
if(getFloatMatrixAsListType ~= "number") then
	error("getFloatMatrixAsList not Float!")
end
callback("setFloatMatrixAsList", getFloatMatrixAsList)
