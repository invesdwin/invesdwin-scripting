arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getBoolean")
if(getBoolean ~= nil) then
	error("getBoolean already defined!")
end
getBoolean = putBoolean
getBooleanType = type(getBoolean)
print(getBooleanType)
print(getBoolean)
if(getBooleanType ~= "boolean") then
	error("getBoolean not Boolean!")
end

print("getBooleanVector")
if(type(getBooleanVector) ~= "nil") then
	error("getBooleanVector already defined!")
end
getBooleanVector = putBooleanVector
getBooleanVectorType = type(getBooleanVector[1])
print(getBooleanVectorType)
print(arraysToString(getBooleanVector))
if(getBooleanVectorType ~= "boolean") then
	error("getBooleanVector not Boolean!")
end

print("getBooleanVectorAsList")
if(type(getBooleanVectorAsList) ~= "nil") then
	error("getBooleanVectorAsList already defined!")
end
getBooleanVectorAsList = putBooleanVectorAsList
getBooleanVectorAsListType = type(getBooleanVectorAsList[1])
print(getBooleanVectorAsListType)
print(arraysToString(getBooleanVectorAsList))
if(getBooleanVectorAsListType ~= "boolean") then
	error("getBooleanVectorAsList not Boolean!")
end

print("getBooleanMatrix")
if(type(getBooleanMatrix) ~= "nil") then
	error("getBooleanMatrix already defined!")
end
getBooleanMatrix = putBooleanMatrix
getBooleanMatrixType = type(getBooleanMatrix[1][1])
print(getBooleanMatrixType)
print(arraysToString(getBooleanMatrix))
if(getBooleanMatrixType ~= "boolean") then
	error("getBooleanMatrix not Boolean!")
end

print("getBooleanMatrixAsList")
if(type(getBooleanMatrixAsList) ~= "nil") then
	error("getBooleanMatrixAsList already defined!")
end
getBooleanMatrixAsList = putBooleanMatrixAsList
getBooleanMatrixAsListType = type(getBooleanMatrixAsList[1][1])
print(getBooleanMatrixAsListType)
print(arraysToString(getBooleanMatrixAsList))
if(getBooleanMatrixAsListType ~= "boolean") then
	error("getBooleanMatrixAsList not Boolean!")
end
