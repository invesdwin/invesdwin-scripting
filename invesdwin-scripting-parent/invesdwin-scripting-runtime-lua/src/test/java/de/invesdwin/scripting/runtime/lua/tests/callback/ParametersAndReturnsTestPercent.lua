arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getPercent")
if(getPercent ~= nil) then
	error("getPercent already defined!")
end
getPercent = callback("getPercent")
getPercentType = type(getPercent)
print(getPercentType)
print(getPercent)
if(getPercentType ~= "number") then
	error("getPercent not Double!")
end
callback("setPercent", getPercent)

print("getPercentVector")
if(getPercentVector ~= nil) then
	error("getPercentVector already defined!")
end
getPercentVector = callback("getPercentVector")
getPercentVectorType = type(getPercentVector[1])
print(getPercentVectorType)
print(arraysToString(getPercentVector))
if(getPercentVectorType ~= "number") then
	error("getPercentVector not Double!")
end
callback("setPercentVector", getPercentVector)

print("getPercentVectorAsList")
if(getPercentVectorAsList ~= nil) then
	error("getPercentVectorAsList already defined!")
end
getPercentVectorAsList = callback("getPercentVectorAsList")
getPercentVectorAsListType = type(getPercentVectorAsList[1])
print(getPercentVectorAsListType)
print(arraysToString(getPercentVectorAsList))
if(getPercentVectorAsListType ~= "number") then
	error("getPercentVectorAsList not Double!")
end
callback("setPercentVectorAsList", getPercentVectorAsList)

print("getPercentMatrix")
if(getPercentMatrix ~= nil) then
	error("getPercentMatrix already defined!")
end
getPercentMatrix = callback("getPercentMatrix")
getPercentMatrixType = type(getPercentMatrix[1][1])
print(getPercentMatrixType)
print(arraysToString(getPercentMatrix))
if(getPercentMatrixType ~= "number") then
	error("getPercentMatrix not Double!")
end
callback("setPercentMatrix", getPercentMatrix)

print("getPercentMatrixAsList")
if(getPercentMatrixAsList ~= nil) then
	error("getPercentMatrixAsList already defined!")
end
getPercentMatrixAsList = callback("getPercentMatrixAsList")
getPercentMatrixAsListType = type(getPercentMatrixAsList[1][1])
print(getPercentMatrixAsListType)
print(arraysToString(getPercentMatrixAsList))
if(getPercentMatrixAsListType ~= "number") then
	error("getPercentMatrixAsList not Double!")
end
callback("setPercentMatrixAsList", getPercentMatrixAsList)
