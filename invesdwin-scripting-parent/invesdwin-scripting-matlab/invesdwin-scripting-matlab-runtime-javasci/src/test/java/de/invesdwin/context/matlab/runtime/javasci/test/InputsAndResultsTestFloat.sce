disp('getFloat')
if exists('getFloat') then
	error('getFloat already defined!')
end
getFloat = putFloat
disp(typeof(getFloat))
disp(getFloat)
if typeof(getFloat)~='constant' then
	error('getFloat not double!')
end

disp('getFloatVector')
if exists('getFloatVector') then
	error('getFloatVector already defined!')
end
getFloatVector = putFloatVector
disp(typeof(getFloatVector))
disp(getFloatVector)
if typeof(getFloatVector)~='constant' then
	error('getFloatVector not double!')
end

disp('getFloatVectorAsList')
if exists('getFloatVectorAsList') then
	error('getFloatVectorAsList already defined!')
end
getFloatVectorAsList = putFloatVectorAsList
disp(typeof(getFloatVectorAsList))
disp(getFloatVectorAsList)
if typeof(getFloatVectorAsList)~='constant' then
	error('getFloatVectorAsList not double!')
end

disp('getFloatMatrix')
if exists('getFloatMatrix') then
	error('getFloatMatrix already defined!')
end
getFloatMatrix = putFloatMatrix
disp(typeof(getFloatMatrix))
disp(getFloatMatrix)
if typeof(getFloatMatrix)~='constant' then
	error('getFloatMatrix not double!')
end

disp('getFloatMatrixAsList')
if exists('getFloatMatrixAsList') then
	error('getFloatMatrixAsList already defined!')
end
getFloatMatrixAsList = putFloatMatrixAsList
disp(typeof(getFloatMatrixAsList))
disp(getFloatMatrixAsList)
if typeof(getFloatMatrixAsList)~='constant' then
	error('getFloatMatrixAsList not double!')
end
