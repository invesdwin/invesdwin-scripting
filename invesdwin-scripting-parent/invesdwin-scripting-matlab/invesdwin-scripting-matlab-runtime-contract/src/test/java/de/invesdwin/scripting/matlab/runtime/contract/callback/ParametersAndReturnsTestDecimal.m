disp('getDecimal')
if exist('getDecimal')
	error('getDecimal already defined!')
end
getDecimal = callback('getDecimal')
disp(class(getDecimal))
disp(getDecimal)
if ~isa(getDecimal, 'double')
	error('getDecimal not double!')
end
callback('setDecimal',getDecimal)

disp('getDecimalVector')
if exist('getDecimalVector')
	error('getDecimalVector already defined!')
end
getDecimalVector = callback('getDecimalVector')
disp(class(getDecimalVector))
disp(getDecimalVector)
if ~isa(getDecimalVector, 'double')
	error('getDecimalVector not double!')
end
callback('setDecimalVector',getDecimalVector)

disp('getDecimalVectorAsList')
if exist('getDecimalVectorAsList')
	error('getDecimalVectorAsList already defined!')
end
getDecimalVectorAsList = callback('getDecimalVectorAsList')
disp(class(getDecimalVectorAsList))
disp(getDecimalVectorAsList)
if ~isa(getDecimalVectorAsList, 'double')
	error('getDecimalVectorAsList not double!')
end
callback('setDecimalVectorAsList',getDecimalVectorAsList)

disp('getDecimalMatrix')
if exist('getDecimalMatrix')
	error('getDecimalMatrix already defined!')
end
getDecimalMatrix = callback('getDecimalMatrix')
disp(class(getDecimalMatrix))
disp(getDecimalMatrix)
if ~isa(getDecimalMatrix, 'double')
	error('getDecimalMatrix not double!')
end
callback('setDecimalMatrix',getDecimalMatrix)

disp('getDecimalMatrixAsList')
if exist('getDecimalMatrixAsList')
	error('getDecimalMatrixAsList already defined!')
end
getDecimalMatrixAsList = callback('getDecimalMatrixAsList')
disp(class(getDecimalMatrixAsList))
disp(getDecimalMatrixAsList)
if ~isa(getDecimalMatrixAsList, 'double')
	error('getDecimalMatrixAsList not double!')
end
callback('setDecimalMatrixAsList',getDecimalMatrixAsList)
