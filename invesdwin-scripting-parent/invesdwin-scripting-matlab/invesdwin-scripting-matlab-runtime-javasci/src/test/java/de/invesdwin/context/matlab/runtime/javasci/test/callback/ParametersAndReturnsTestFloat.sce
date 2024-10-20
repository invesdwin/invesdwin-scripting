disp('getFloat')
if exists('getFloat') then
	error('getFloat already defined!')
end
getFloat = callback('getFloat')
disp(typeof(getFloat))
disp(getFloat)
if typeof(getFloat)~='constant' then
	error('getFloat not double!')
end
callback('setFloat',getFloat)

disp('getFloatVector')
if exists('getFloatVector') then
	error('getFloatVector already defined!')
end
getFloatVector = callback('getFloatVector')
disp(typeof(getFloatVector))
disp(getFloatVector)
if typeof(getFloatVector)~='constant' then
	error('getFloatVector not double!')
end
callback('setFloatVector',getFloatVector)

disp('getFloatVectorAsList')
if exists('getFloatVectorAsList') then
	error('getFloatVectorAsList already defined!')
end
getFloatVectorAsList = callback('getFloatVectorAsList')
disp(typeof(getFloatVectorAsList))
disp(getFloatVectorAsList)
if typeof(getFloatVectorAsList)~='constant' then
	error('getFloatVectorAsList not double!')
end
callback('setFloatVectorAsList',getFloatVectorAsList)

disp('getFloatMatrix')
if exists('getFloatMatrix') then
	error('getFloatMatrix already defined!')
end
getFloatMatrix = callback('getFloatMatrix')
disp(typeof(getFloatMatrix))
disp(getFloatMatrix)
if typeof(getFloatMatrix)~='constant' then
	error('getFloatMatrix not double!')
end
callback('setFloatMatrix',getFloatMatrix)

disp('getFloatMatrixAsList')
if exists('getFloatMatrixAsList') then
	error('getFloatMatrixAsList already defined!')
end
getFloatMatrixAsList = callback('getFloatMatrixAsList')
disp(typeof(getFloatMatrixAsList))
disp(getFloatMatrixAsList)
if typeof(getFloatMatrixAsList)~='constant' then
	error('getFloatMatrixAsList not double!')
end
callback('setFloatMatrixAsList',getFloatMatrixAsList)
