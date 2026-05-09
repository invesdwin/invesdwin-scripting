arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getDecimal")
if(getDecimal ~= nil) then
	error("getDecimal already defined!")
end
getDecimal = putDecimal
getDecimalType = type(getDecimal)
print(getDecimalType)
print(getDecimal)
if(getDecimalType ~= "number") then
	error("getDecimal not Double!")
end

print("getDecimalVector")
if(getDecimalVector ~= nil) then
	error("getDecimalVector already defined!")
end
getDecimalVector = putDecimalVector
getDecimalVectorType = type(getDecimalVector[1])
print(getDecimalVectorType)
print(arraysToString(getDecimalVector))
if(getDecimalVectorType ~= "number") then
	error("getDecimalVector not Double!")
end

print("getDecimalVectorAsList")
if(getDecimalVectorAsList ~= nil) then
	error("getDecimalVectorAsList already defined!")
end
getDecimalVectorAsList = putDecimalVectorAsList
getDecimalVectorAsListType = type(getDecimalVectorAsList[1])
print(getDecimalVectorAsListType)
print(arraysToString(getDecimalVectorAsList))
if(getDecimalVectorAsListType ~= "number") then
	error("getDecimalVectorAsList not Double!")
end

print("getDecimalMatrix")
if(getDecimalMatrix ~= nil) then
	error("getDecimalMatrix already defined!")
end
getDecimalMatrix = putDecimalMatrix
getDecimalMatrixType = type(getDecimalMatrix[1][1])
print(getDecimalMatrixType)
print(arraysToString(getDecimalMatrix))
if(getDecimalMatrixType ~= "number") then
	error("getDecimalMatrix not Double!")
end

print("getDecimalMatrixAsList")
if(getDecimalMatrixAsList ~= nil) then
	error("getDecimalMatrixAsList already defined!")
end
getDecimalMatrixAsList = putDecimalMatrixAsList
getDecimalMatrixAsListType = type(getDecimalMatrixAsList[1][1])
print(getDecimalMatrixAsListType)
print(arraysToString(getDecimalMatrixAsList))
if(getDecimalMatrixAsListType ~= "number") then
	error("getDecimalMatrixAsList not Double!")
end
