disp('getFloat')
if exist('getFloat')
	error('getFloat already defined!')
end
getFloat = callback('getFloat')
disp(class(getFloat))
disp(getFloat)
if ~isa(getFloat, 'single')
	error('getFloat not single!')
end
callback('setFloat',getFloat)

disp('getFloatVector')
if exist('getFloatVector')
	error('getFloatVector already defined!')
end
getFloatVector = callback('getFloatVector')
disp(class(getFloatVector))
disp(getFloatVector)
if ~isa(getFloatVector, 'single')
	error('getFloatVector not single!')
end
callback('setFloatVector',getFloatVector)

disp('getFloatVectorAsList')
if exist('getFloatVectorAsList')
	error('getFloatVectorAsList already defined!')
end
getFloatVectorAsList = callback('getFloatVectorAsList')
disp(class(getFloatVectorAsList))
disp(getFloatVectorAsList)
if ~isa(getFloatVectorAsList, 'single')
	error('getFloatVectorAsList not single!')
end
callback('setFloatVectorAsList',getFloatVectorAsList)

disp('getFloatMatrix')
if exist('getFloatMatrix')
	error('getFloatMatrix already defined!')
end
getFloatMatrix = callback('getFloatMatrix')
disp(class(getFloatMatrix))
disp(getFloatMatrix)
if ~isa(getFloatMatrix, 'single')
	error('getFloatMatrix not single!')
end
callback('setFloatMatrix',getFloatMatrix)

disp('getFloatMatrixAsList')
if exist('getFloatMatrixAsList')
	error('getFloatMatrixAsList already defined!')
end
getFloatMatrixAsList = callback('getFloatMatrixAsList')
disp(class(getFloatMatrixAsList))
disp(getFloatMatrixAsList)
if ~isa(getFloatMatrixAsList, 'single')
	error('getFloatMatrixAsList not single!')
end
callback('setFloatMatrixAsList',getFloatMatrixAsList)
