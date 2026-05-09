arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getDouble")
if(getDouble ~= nil) then
	error("getDouble already defined!")
end
getDouble = putDouble
getDoubleType = type(getDouble)
print(getDoubleType)
print(getDouble)
if(getDoubleType ~= "number") then
	error("getDouble not Double!")
end

print("getDoubleVector")
if(getDoubleVector ~= nil) then
	error("getDoubleVector already defined!")
end
getDoubleVector = putDoubleVector
getDoubleVectorType = type(getDoubleVector[1])
print(getDoubleVectorType)
print(arraysToString(getDoubleVector))
if(getDoubleVectorType ~= "number") then
	error("getDoubleVector not Double!")
end

print("getDoubleVectorAsList")
if(getDoubleVectorAsList ~= nil) then
	error("getDoubleVectorAsList already defined!")
end
getDoubleVectorAsList = putDoubleVectorAsList
getDoubleVectorAsListType = type(getDoubleVectorAsList[1])
print(getDoubleVectorAsListType)
print(arraysToString(getDoubleVectorAsList))
if(getDoubleVectorAsListType ~= "number") then
	error("getDoubleVectorAsList not Double!")
end

print("getDoubleMatrix")
if(getDoubleMatrix ~= nil) then
	error("getDoubleMatrix already defined!")
end
getDoubleMatrix = putDoubleMatrix
getDoubleMatrixType = type(getDoubleMatrix[1][1])
print(getDoubleMatrixType)
print(arraysToString(getDoubleMatrix))
if(getDoubleMatrixType ~= "number") then
	error("getDoubleMatrix not Double!")
end

print("getDoubleMatrixAsList")
if(getDoubleMatrixAsList ~= nil) then
	error("getDoubleMatrixAsList already defined!")
end
getDoubleMatrixAsList = putDoubleMatrixAsList
getDoubleMatrixAsListType = type(getDoubleMatrixAsList[1][1])
print(getDoubleMatrixAsListType)
print(arraysToString(getDoubleMatrixAsList))
if(getDoubleMatrixAsListType ~= "number") then
	error("getDoubleMatrixAsList not Double!")
end
