disp('getDouble')
if exist('getDouble')
	error('getDouble already defined!')
end
getDouble = callback('getDouble')
disp(class(getDouble))
disp(getDouble)
if ~isa(getDouble, 'double')
	error('getDouble not double!')
end
callback('setDouble',getDouble)

disp('getDoubleVector')
if exist('getDoubleVector')
	error('getDoubleVector already defined!')
end
getDoubleVector = callback('getDoubleVector')
disp(class(getDoubleVector))
disp(getDoubleVector)
if ~isa(getDoubleVector, 'double')
	error('getDoubleVector not double!')
end
callback('setDoubleVector',getDoubleVector)

disp('getDoubleVectorAsList')
if exist('getDoubleVectorAsList')
	error('getDoubleVectorAsList already defined!')
end
getDoubleVectorAsList = callback('getDoubleVectorAsList')
disp(class(getDoubleVectorAsList))
disp(getDoubleVectorAsList)
if ~isa(getDoubleVectorAsList, 'double')
	error('getDoubleVectorAsList not double!')
end
callback('setDoubleVectorAsList',getDoubleVectorAsList)

disp('getDoubleMatrix')
if exist('getDoubleMatrix')
	error('getDoubleMatrix already defined!')
end
getDoubleMatrix = callback('getDoubleMatrix')
disp(class(getDoubleMatrix))
disp(getDoubleMatrix)
if ~isa(getDoubleMatrix, 'double')
	error('getDoubleMatrix not double!')
end
callback('setDoubleMatrix',getDoubleMatrix)

disp('getDoubleMatrixAsList')
if exist('getDoubleMatrixAsList')
	error('getDoubleMatrixAsList already defined!')
end
getDoubleMatrixAsList = callback('getDoubleMatrixAsList')
disp(class(getDoubleMatrixAsList))
disp(getDoubleMatrixAsList)
if ~isa(getDoubleMatrixAsList, 'double')
	error('getDoubleMatrixAsList not double!')
end
callback('setDoubleMatrixAsList',getDoubleMatrixAsList)
