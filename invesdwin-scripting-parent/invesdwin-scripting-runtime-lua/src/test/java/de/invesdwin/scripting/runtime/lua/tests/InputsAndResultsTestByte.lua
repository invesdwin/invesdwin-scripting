arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getByte")
if(type(getByte) ~= "nil") then
	error("getByte already defined!")
end
getByte = putByte
getByteType = type(getByte)
print(getByteType)
print(getByte)
if(getByteType ~= "number") then
	error("getByte not Byte!")
end

print("getByteVector")
if(type(getByteVector) ~= "nil") then
	error("getByteVector already defined!")
end
getByteVector = putByteVector
getByteVectorType = type(getByteVector[1])
print(getByteVectorType)
print(arraysToString(getByteVector))
if(getByteVectorType ~= "number") then
	error("getByteVector not Byte!")
end

print("getByteVectorAsList")
if(type(getByteVectorAsList) ~= "nil") then
	error("getByteVectorAsList already defined!")
end
getByteVectorAsList = putByteVectorAsList
getByteVectorAsListType = type(getByteVectorAsList[1])
print(getByteVectorAsListType)
print(arraysToString(getByteVectorAsList))
if(getByteVectorAsListType ~= "number") then
	error("getByteVectorAsList not Byte!")
end

print("getByteMatrix")
if(type(getByteMatrix) ~= "nil") then
	error("getByteMatrix already defined!")
end
getByteMatrix = putByteMatrix
getByteMatrixType = type(getByteMatrix[1][1])
print(getByteMatrixType)
print(arraysToString(getByteMatrix))
if(getByteMatrixType ~= "number") then
	error("getByteMatrix not Byte!")
end

print("getByteMatrixAsList")
if(type(getByteMatrixAsList) ~= "nil") then
	error("getByteMatrixAsList already defined!")
end
getByteMatrixAsList = putByteMatrixAsList
getByteMatrixAsListType = type(getByteMatrixAsList[1][1])
print(getByteMatrixAsListType)
print(arraysToString(getByteMatrixAsList))
if(getByteMatrixAsListType ~= "number") then
	error("getByteMatrixAsList not Byte!")
end
