arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getShort")
if(getShort ~= nil) then
	error("getShort already defined!")
end
getShort = callback("getShort")
getShortType = type(getShort)
print(getShortType)
print(getShort)
if(getShortType ~= "number") then
	error("getShort not Short!")
end
callback("setShort", getShort)

print("getShortVector")
if(getShortVector ~= nil) then
	error("getShortVector already defined!")
end
getShortVector = callback("getShortVector")
getShortVectorType = type(getShortVector[1])
print(getShortVectorType)
print(arraysToString(getShortVector))
if(getShortVectorType ~= "number") then
	error("getShortVector not Short!")
end
callback("setShortVector", getShortVector)

print("getShortVectorAsList")
if(getShortVectorAsList ~= nil) then
	error("getShortVectorAsList already defined!")
end
getShortVectorAsList = callback("getShortVectorAsList")
getShortVectorAsListType = type(getShortVectorAsList[1])
print(getShortVectorAsListType)
print(arraysToString(getShortVectorAsList))
if(getShortVectorAsListType ~= "number") then
	error("getShortVectorAsList not Short!")
end
callback("setShortVectorAsList", getShortVectorAsList)

print("getShortMatrix")
if(getShortMatrix ~= nil) then
	error("getShortMatrix already defined!")
end
getShortMatrix = callback("getShortMatrix")
getShortMatrixType = type(getShortMatrix[1][1])
print(getShortMatrixType)
print(arraysToString(getShortMatrix))
if(getShortMatrixType ~= "number") then
	error("getShortMatrix not Short!")
end
callback("setShortMatrix", getShortMatrix)

print("getShortMatrixAsList")
if(getShortMatrixAsList ~= nil) then
	error("getShortMatrixAsList already defined!")
end
getShortMatrixAsList = callback("getShortMatrixAsList")
getShortMatrixAsListType = type(getShortMatrixAsList[1][1])
print(getShortMatrixAsListType)
print(arraysToString(getShortMatrixAsList))
if(getShortMatrixAsListType ~= "number") then
	error("getShortMatrixAsList not Short!")
end
callback("setShortMatrixAsList", getShortMatrixAsList)
