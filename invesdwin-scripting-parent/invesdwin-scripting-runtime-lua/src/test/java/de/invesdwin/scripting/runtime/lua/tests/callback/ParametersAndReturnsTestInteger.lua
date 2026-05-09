arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getInteger")
if(getInteger ~= nil) then
	error("getInteger already defined!")
end
getInteger = callback("getInteger")
getIntegerType = type(getInteger)
print(getIntegerType)
print(getInteger)
if(getIntegerType ~= "number") then
	error("getInteger not Integer!")
end
callback("setInteger", getInteger)

print("getIntegerVector")
if(getIntegerVector ~= nil) then
	error("getIntegerVector already defined!")
end
getIntegerVector = callback("getIntegerVector")
getIntegerVectorType = type(getIntegerVector[1])
print(getIntegerVectorType)
print(arraysToString(getIntegerVector))
if(getIntegerVectorType ~= "number") then
	error("getIntegerVector not Integer!")
end
callback("setIntegerVector", getIntegerVector)

print("getIntegerVectorAsList")
if(getIntegerVectorAsList ~= nil) then
	error("getIntegerVectorAsList already defined!")
end
getIntegerVectorAsList = callback("getIntegerVectorAsList")
getIntegerVectorAsListType = type(getIntegerVectorAsList[1])
print(getIntegerVectorAsListType)
print(arraysToString(getIntegerVectorAsList))
if(getIntegerVectorAsListType ~= "number") then
	error("getIntegerVectorAsList not Integer!")
end
callback("setIntegerVectorAsList", getIntegerVectorAsList)

print("getIntegerMatrix")
if(getIntegerMatrix ~= nil) then
	error("getIntegerMatrix already defined!")
end
getIntegerMatrix = callback("getIntegerMatrix")
getIntegerMatrixType = type(getIntegerMatrix[1][1])
print(getIntegerMatrixType)
print(arraysToString(getIntegerMatrix))
if(getIntegerMatrixType ~= "number") then
	error("getIntegerMatrix not Integer!")
end
callback("setIntegerMatrix", getIntegerMatrix)

print("getIntegerMatrixAsList")
if(getIntegerMatrixAsList ~= nil) then
	error("getIntegerMatrixAsList already defined!")
end
getIntegerMatrixAsList = callback("getIntegerMatrixAsList")
getIntegerMatrixAsListType = type(getIntegerMatrixAsList[1][1])
print(getIntegerMatrixAsListType)
print(arraysToString(getIntegerMatrixAsList))
if(getIntegerMatrixAsListType ~= "number") then
	error("getIntegerMatrixAsList not Integer!")
end
callback("setIntegerMatrixAsList", getIntegerMatrixAsList)
