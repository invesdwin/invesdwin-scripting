arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getInteger")
if(type(getInteger) ~= "nil") then
	error("getInteger already defined!")
end
getInteger = putInteger
getIntegerType = type(getInteger)
print(getIntegerType)
print(getInteger)
if(getIntegerType ~= "number") then
	error("getInteger not Integer!")
end

print("getIntegerVector")
if(type(getIntegerVector) ~= "nil") then
	error("getIntegerVector already defined!")
end
getIntegerVector = putIntegerVector
getIntegerVectorType = type(getIntegerVector[1])
print(getIntegerVectorType)
print(arraysToString(getIntegerVector))
if(getIntegerVectorType ~= "number") then
	error("getIntegerVector not Integer!")
end

print("getIntegerVectorAsList")
if(type(getIntegerVectorAsList) ~= "nil") then
	error("getIntegerVectorAsList already defined!")
end
getIntegerVectorAsList = putIntegerVectorAsList
getIntegerVectorAsListType = type(getIntegerVectorAsList[1])
print(getIntegerVectorAsListType)
print(arraysToString(getIntegerVectorAsList))
if(getIntegerVectorAsListType ~= "number") then
	error("getIntegerVectorAsList not Integer!")
end

print("getIntegerMatrix")
if(type(getIntegerMatrix) ~= "nil") then
	error("getIntegerMatrix already defined!")
end
getIntegerMatrix = putIntegerMatrix
getIntegerMatrixType = type(getIntegerMatrix[1][1])
print(getIntegerMatrixType)
print(arraysToString(getIntegerMatrix))
if(getIntegerMatrixType ~= "number") then
	error("getIntegerMatrix not Integer!")
end

print("getIntegerMatrixAsList")
if(type(getIntegerMatrixAsList) ~= "nil") then
	error("getIntegerMatrixAsList already defined!")
end
getIntegerMatrixAsList = putIntegerMatrixAsList
getIntegerMatrixAsListType = type(getIntegerMatrixAsList[1][1])
print(getIntegerMatrixAsListType)
print(arraysToString(getIntegerMatrixAsList))
if(getIntegerMatrixAsListType ~= "number") then
	error("getIntegerMatrixAsList not Integer!")
end
