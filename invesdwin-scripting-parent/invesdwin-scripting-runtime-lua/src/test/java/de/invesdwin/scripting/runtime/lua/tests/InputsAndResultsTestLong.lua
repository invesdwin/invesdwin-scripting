arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getLong")
if(type(getLong) ~= "nil") then
	error("getLong already defined!")
end
getLong = putLong
getLongType = type(getLong)
print(getLongType)
print(getLong)
if(getLongType ~= "number") then
	error("getLong not Long!")
end

print("getLongVector")
if(type(getLongVector) ~= "nil") then
	error("getLongVector already defined!")
end
getLongVector = putLongVector
getLongVectorType = type(getLongVector[1])
print(getLongVectorType)
print(arraysToString(getLongVector))
if(getLongVectorType ~= "number") then
	error("getLongVector not Long!")
end

print("getLongVectorAsList")
if(type(getLongVectorAsList) ~= "nil") then
	error("getLongVectorAsList already defined!")
end
getLongVectorAsList = putLongVectorAsList
getLongVectorAsListType = type(getLongVectorAsList[1])
print(getLongVectorAsListType)
print(arraysToString(getLongVectorAsList))
if(getLongVectorAsListType ~= "number") then
	error("getLongVectorAsList not Long!")
end

print("getLongMatrix")
if(type(getLongMatrix) ~= "nil") then
	error("getLongMatrix already defined!")
end
getLongMatrix = putLongMatrix
getLongMatrixType = type(getLongMatrix[1][1])
print(getLongMatrixType)
print(arraysToString(getLongMatrix))
if(getLongMatrixType ~= "number") then
	error("getLongMatrix not Long!")
end

print("getLongMatrixAsList")
if(type(getLongMatrixAsList) ~= "nil") then
	error("getLongMatrixAsList already defined!")
end
getLongMatrixAsList = putLongMatrixAsList
getLongMatrixAsListType = type(getLongMatrixAsList[1][1])
print(getLongMatrixAsListType)
print(arraysToString(getLongMatrixAsList))
if(getLongMatrixAsListType ~= "number") then
	error("getLongMatrixAsList not Long!")
end
