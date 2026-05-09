arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getDecimal")
if(getDecimal ~= nil) then
	error("getDecimal already defined!")
end
getDecimal = callback("getDecimal")
getDecimalType = type(getDecimal)
print(getDecimalType)
print(getDecimal)
if(getDecimalType ~= "number") then
	error("getDecimal not Double!")
end
callback("setDecimal", getDecimal)

print("getDecimalVector")
if(getDecimalVector ~= nil) then
	error("getDecimalVector already defined!")
end
getDecimalVector = callback("getDecimalVector")
getDecimalVectorType = type(getDecimalVector[1])
print(getDecimalVectorType)
print(arraysToString(getDecimalVector))
if(getDecimalVectorType ~= "number") then
	error("getDecimalVector not Double!")
end
callback("setDecimalVector", getDecimalVector)

print("getDecimalVectorAsList")
if(getDecimalVectorAsList ~= nil) then
	error("getDecimalVectorAsList already defined!")
end
getDecimalVectorAsList = callback("getDecimalVectorAsList")
getDecimalVectorAsListType = type(getDecimalVectorAsList[1])
print(getDecimalVectorAsListType)
print(arraysToString(getDecimalVectorAsList))
if(getDecimalVectorAsListType ~= "number") then
	error("getDecimalVectorAsList not Double!")
end
callback("setDecimalVectorAsList", getDecimalVectorAsList)

print("getDecimalMatrix")
if(getDecimalMatrix ~= nil) then
	error("getDecimalMatrix already defined!")
end
getDecimalMatrix = callback("getDecimalMatrix")
getDecimalMatrixType = type(getDecimalMatrix[1][1])
print(getDecimalMatrixType)
print(arraysToString(getDecimalMatrix))
if(getDecimalMatrixType ~= "number") then
	error("getDecimalMatrix not Double!")
end
callback("setDecimalMatrix", getDecimalMatrix)

print("getDecimalMatrixAsList")
if(getDecimalMatrixAsList ~= nil) then
	error("getDecimalMatrixAsList already defined!")
end
getDecimalMatrixAsList = callback("getDecimalMatrixAsList")
getDecimalMatrixAsListType = type(getDecimalMatrixAsList[1][1])
print(getDecimalMatrixAsListType)
print(arraysToString(getDecimalMatrixAsList))
if(getDecimalMatrixAsListType ~= "number") then
	error("getDecimalMatrixAsList not Double!")
end
callback("setDecimalMatrixAsList", getDecimalMatrixAsList)
