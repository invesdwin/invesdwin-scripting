arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getByte")
if(getByte ~= nil) then
	error("getByte already defined!")
end
getByte = callback("getByte")
getByteType = type(getByte)
print(getByteType)
print(getByte)
if(getByteType ~= "number") then
	error("getByte not Byte!")
end
callback("setByte", getByte)

print("getByteVector")
if(getByteVector ~= nil) then
	error("getByteVector already defined!")
end
getByteVector = callback("getByteVector")
getByteVectorType = type(getByteVector[1])
print(getByteVectorType)
print(arraysToString(getByteVector))
if(getByteVectorType ~= "number") then
	error("getByteVector not Byte!")
end
callback("setByteVector", getByteVector)

print("getByteVectorAsList")
if(getByteVectorAsList ~= nil) then
	error("getByteVectorAsList already defined!")
end
getByteVectorAsList = callback("getByteVectorAsList")
getByteVectorAsListType = type(getByteVectorAsList[1])
print(getByteVectorAsListType)
print(arraysToString(getByteVectorAsList))
if(getByteVectorAsListType ~= "number") then
	error("getByteVectorAsList not Byte!")
end
callback("setByteVectorAsList", getByteVectorAsList)

print("getByteMatrix")
if(getByteMatrix ~= nil) then
	error("getByteMatrix already defined!")
end
getByteMatrix = callback("getByteMatrix")
getByteMatrixType = type(getByteMatrix[1][1])
print(getByteMatrixType)
print(arraysToString(getByteMatrix))
if(getByteMatrixType ~= "number") then
	error("getByteMatrix not Byte!")
end
callback("setByteMatrix", getByteMatrix)

print("getByteMatrixAsList")
if(getByteMatrixAsList ~= nil) then
	error("getByteMatrixAsList already defined!")
end
getByteMatrixAsList = callback("getByteMatrixAsList")
getByteMatrixAsListType = type(getByteMatrixAsList[1][1])
print(getByteMatrixAsListType)
print(arraysToString(getByteMatrixAsList))
if(getByteMatrixAsListType ~= "number") then
	error("getByteMatrixAsList not Byte!")
end
callback("setByteMatrixAsList", getByteMatrixAsList)
