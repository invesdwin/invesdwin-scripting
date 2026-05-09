arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getLong")
if(getLong ~= nil) then
	error("getLong already defined!")
end
getLong = callback("getLong")
getLongType = type(getLong)
print(getLongType)
print(getLong)
if(getLongType ~= "number") then
	error("getLong not Long!")
end
callback("setLong", getLong)

print("getLongVector")
if(getLongVector ~= nil) then
	error("getLongVector already defined!")
end
getLongVector = callback("getLongVector")
getLongVectorType = type(getLongVector[1])
print(getLongVectorType)
print(arraysToString(getLongVector))
if(getLongVectorType ~= "number") then
	error("getLongVector not Long!")
end
callback("setLongVector", getLongVector)

print("getLongVectorAsList")
if(getLongVectorAsList ~= nil) then
	error("getLongVectorAsList already defined!")
end
getLongVectorAsList = callback("getLongVectorAsList")
getLongVectorAsListType = type(getLongVectorAsList[1])
print(getLongVectorAsListType)
print(arraysToString(getLongVectorAsList))
if(getLongVectorAsListType ~= "number") then
	error("getLongVectorAsList not Long!")
end
callback("setLongVectorAsList", getLongVectorAsList)

print("getLongMatrix")
if(getLongMatrix ~= nil) then
	error("getLongMatrix already defined!")
end
getLongMatrix = callback("getLongMatrix")
getLongMatrixType = type(getLongMatrix[1][1])
print(getLongMatrixType)
print(arraysToString(getLongMatrix))
if(getLongMatrixType ~= "number") then
	error("getLongMatrix not Long!")
end
callback("setLongMatrix", getLongMatrix)

print("getLongMatrixAsList")
if(getLongMatrixAsList ~= nil) then
	error("getLongMatrixAsList already defined!")
end
getLongMatrixAsList = callback("getLongMatrixAsList")
getLongMatrixAsListType = type(getLongMatrixAsList[1][1])
print(getLongMatrixAsListType)
print(arraysToString(getLongMatrixAsList))
if(getLongMatrixAsListType ~= "number") then
	error("getLongMatrixAsList not Long!")
end
callback("setLongMatrixAsList", getLongMatrixAsList)
