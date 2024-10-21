disp('getFloat')
if exist('getFloat')
	error('getFloat already defined!')
end
getFloat = putFloat
disp(class(getFloat))
disp(getFloat)
if ~isa(getFloat, 'single')
	error('getFloat not single!')
end

disp('getFloatVector')
if exist('getFloatVector')
	error('getFloatVector already defined!')
end
getFloatVector = putFloatVector
disp(class(getFloatVector))
disp(getFloatVector)
if ~isa(getFloatVector, 'single')
	error('getFloatVector not single!')
end

disp('getFloatVectorAsList')
if exist('getFloatVectorAsList')
	error('getFloatVectorAsList already defined!')
end
getFloatVectorAsList = putFloatVectorAsList
disp(class(getFloatVectorAsList))
disp(getFloatVectorAsList)
if ~isa(getFloatVectorAsList, 'single')
	error('getFloatVectorAsList not single!')
end

disp('getFloatMatrix')
if exist('getFloatMatrix')
	error('getFloatMatrix already defined!')
end
getFloatMatrix = putFloatMatrix
disp(class(getFloatMatrix))
disp(getFloatMatrix)
if ~isa(getFloatMatrix, 'single')
	error('getFloatMatrix not single!')
end

disp('getFloatMatrixAsList')
if exist('getFloatMatrixAsList')
	error('getFloatMatrixAsList already defined!')
end
getFloatMatrixAsList = putFloatMatrixAsList
disp(class(getFloatMatrixAsList))
disp(getFloatMatrixAsList)
if ~isa(getFloatMatrixAsList, 'single')
	error('getFloatMatrixAsList not single!')
end
