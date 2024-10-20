disp('getDouble')
if exist('getDouble')
	error('getDouble already defined!')
end
getDouble = putDouble
disp(class(getDouble))
disp(getDouble)
if ~isa(getDouble, 'double')
	error('getDouble not double!')
end

disp('getDoubleVector')
if exist('getDoubleVector')
	error('getDoubleVector already defined!')
end
getDoubleVector = putDoubleVector
disp(class(getDoubleVector))
disp(getDoubleVector)
if ~isa(getDoubleVector, 'double')
	error('getDoubleVector not double!')
end

disp('getDoubleVectorAsList')
if exist('getDoubleVectorAsList')
	error('getDoubleVectorAsList already defined!')
end
getDoubleVectorAsList = putDoubleVectorAsList
disp(class(getDoubleVectorAsList))
disp(getDoubleVectorAsList)
if ~isa(getDoubleVectorAsList, 'double')
	error('getDoubleVectorAsList not double!')
end

disp('getDoubleMatrix')
if exist('getDoubleMatrix')
	error('getDoubleMatrix already defined!')
end
getDoubleMatrix = putDoubleMatrix
disp(class(getDoubleMatrix))
disp(getDoubleMatrix)
if ~isa(getDoubleMatrix, 'double')
	error('getDoubleMatrix not double!')
end

disp('getDoubleMatrixAsList')
if exist('getDoubleMatrixAsList')
	error('getDoubleMatrixAsList already defined!')
end
getDoubleMatrixAsList = putDoubleMatrixAsList
disp(class(getDoubleMatrixAsList))
disp(getDoubleMatrixAsList)
if ~isa(getDoubleMatrixAsList, 'double')
	error('getDoubleMatrixAsList not double!')
end
