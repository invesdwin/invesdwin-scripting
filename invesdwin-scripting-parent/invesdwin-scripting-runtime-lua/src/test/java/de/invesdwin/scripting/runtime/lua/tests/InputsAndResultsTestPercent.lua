arraysToString = java.method(java.import('de.invesdwin.util.collections.Arrays'),'toString','java.lang.Object')

print("getPercent")
if(getPercent ~= nil) then
	error("getPercent already defined!")
end
getPercent = putPercent
getPercentType = type(getPercent)
print(getPercentType)
print(getPercent)
if(getPercentType ~= "number") then
	error("getPercent not Double!")
end

print("getPercentVector")
if(getPercentVector ~= nil) then
	error("getPercentVector already defined!")
end
getPercentVector = putPercentVector
getPercentVectorType = type(getPercentVector[1])
print(getPercentVectorType)
print(arraysToString(getPercentVector))
if(getPercentVectorType ~= "number") then
	error("getPercentVector not Double!")
end

print("getPercentVectorAsList")
if(getPercentVectorAsList ~= nil) then
	error("getPercentVectorAsList already defined!")
end
getPercentVectorAsList = putPercentVectorAsList
getPercentVectorAsListType = type(getPercentVectorAsList[1])
print(getPercentVectorAsListType)
print(arraysToString(getPercentVectorAsList))
if(getPercentVectorAsListType ~= "number") then
	error("getPercentVectorAsList not Double!")
end

print("getPercentMatrix")
if(getPercentMatrix ~= nil) then
	error("getPercentMatrix already defined!")
end
getPercentMatrix = putPercentMatrix
getPercentMatrixType = type(getPercentMatrix[1][1])
print(getPercentMatrixType)
print(arraysToString(getPercentMatrix))
if(getPercentMatrixType ~= "number") then
	error("getPercentMatrix not Double!")
end

print("getPercentMatrixAsList")
if(getPercentMatrixAsList ~= nil) then
	error("getPercentMatrixAsList already defined!")
end
getPercentMatrixAsList = putPercentMatrixAsList
getPercentMatrixAsListType = type(getPercentMatrixAsList[1][1])
print(getPercentMatrixAsListType)
print(arraysToString(getPercentMatrixAsList))
if(getPercentMatrixAsListType ~= "number") then
	error("getPercentMatrixAsList not Double!")
end
