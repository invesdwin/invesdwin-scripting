arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getBoolean")
if(getBoolean ~= nil) then
	error("getBoolean already defined!")
end
getBoolean = callback("getBoolean")
getBooleanType = type(getBoolean)
print(getBooleanType)
print(getBoolean)
if(getBooleanType ~= "boolean") then
	error("getBoolean not Boolean!")
end
callback("setBoolean", getBoolean)

print("getBooleanVector")
if(getBooleanVector ~= nil) then
	error("getBooleanVector already defined!")
end
getBooleanVector = callback("getBooleanVector")
getBooleanVectorType = type(getBooleanVector[1])
print(getBooleanVectorType)
print(arraysToString(getBooleanVector))
if(getBooleanVectorType ~= "boolean") then
	error("getBooleanVector not Boolean!")
end
callback("setBooleanVector", getBooleanVector)

print("getBooleanVectorAsList")
if(getBooleanVectorAsList ~= nil) then
	error("getBooleanVectorAsList already defined!")
end
getBooleanVectorAsList = callback("getBooleanVectorAsList")
getBooleanVectorAsListType = type(getBooleanVectorAsList[1])
print(getBooleanVectorAsListType)
print(arraysToString(getBooleanVectorAsList))
if(getBooleanVectorAsListType ~= "boolean") then
	error("getBooleanVectorAsList not Boolean!")
end
callback("setBooleanVectorAsList", getBooleanVectorAsList)

print("getBooleanMatrix")
if(getBooleanMatrix ~= nil) then
	error("getBooleanMatrix already defined!")
end
getBooleanMatrix = callback("getBooleanMatrix")
getBooleanMatrixType = type(getBooleanMatrix[1][1])
print(getBooleanMatrixType)
print(arraysToString(getBooleanMatrix))
if(getBooleanMatrixType ~= "boolean") then
	error("getBooleanMatrix not Boolean!")
end
callback("setBooleanMatrix", getBooleanMatrix)

print("getBooleanMatrixAsList")
if(getBooleanMatrixAsList ~= nil) then
	error("getBooleanMatrixAsList already defined!")
end
getBooleanMatrixAsList = callback("getBooleanMatrixAsList")
getBooleanMatrixAsListType = type(getBooleanMatrixAsList[1][1])
print(getBooleanMatrixAsListType)
print(arraysToString(getBooleanMatrixAsList))
if(getBooleanMatrixAsListType ~= "boolean") then
	error("getBooleanMatrixAsList not Boolean!")
end
callback("setBooleanMatrixAsList", getBooleanMatrixAsList)
