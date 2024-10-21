disp('getDecimal')
if exist('getDecimal')
	error('getDecimal already defined!')
end
getDecimal = putDecimal
disp(class(getDecimal))
disp(getDecimal)
if ~isa(getDecimal, 'double')
	error('getDecimal not double!')
end

disp('getDecimalVector')
if exist('getDecimalVector')
	error('getDecimalVector already defined!')
end
getDecimalVector = putDecimalVector
disp(class(getDecimalVector))
disp(getDecimalVector)
if ~isa(getDecimalVector, 'double')
	error('getDecimalVector not double!')
end

disp('getDecimalVectorAsList')
if exist('getDecimalVectorAsList')
	error('getDecimalVectorAsList already defined!')
end
getDecimalVectorAsList = putDecimalVectorAsList
disp(class(getDecimalVectorAsList))
disp(getDecimalVectorAsList)
if ~isa(getDecimalVectorAsList, 'double')
	error('getDecimalVectorAsList not double!')
end

disp('getDecimalMatrix')
if exist('getDecimalMatrix')
	error('getDecimalMatrix already defined!')
end
getDecimalMatrix = putDecimalMatrix
disp(class(getDecimalMatrix))
disp(getDecimalMatrix)
if ~isa(getDecimalMatrix, 'double')
	error('getDecimalMatrix not double!')
end

disp('getDecimalMatrixAsList')
if exist('getDecimalMatrixAsList')
	error('getDecimalMatrixAsList already defined!')
end
getDecimalMatrixAsList = putDecimalMatrixAsList
disp(class(getDecimalMatrixAsList))
disp(getDecimalMatrixAsList)
if ~isa(getDecimalMatrixAsList, 'double')
	error('getDecimalMatrixAsList not double!')
end
