disp('getShort')
if exist('getShort')
	error('getShort already defined!')
end
getShort = putShort
disp(class(getShort))
disp(getShort)
if ~isa(getShort, 'int16')
	error('getShort not int16!')
end

disp('getShortVector')
if exist('getShortVector')
	error('getShortVector already defined!')
end
getShortVector = putShortVector
disp(class(getShortVector))
disp(getShortVector)
if ~isa(getShortVector, 'int16')
	error('getShortVector not int16!')
end

disp('getShortVectorAsList')
if exist('getShortVectorAsList')
	error('getShortVectorAsList already defined!')
end
getShortVectorAsList = putShortVectorAsList
disp(class(getShortVectorAsList))
disp(getShortVectorAsList)
if ~isa(getShortVectorAsList, 'int16')
	error('getShortVectorAsList not int16!')
end

disp('getShortMatrix')
if exist('getShortMatrix')
	error('getShortMatrix already defined!')
end
getShortMatrix = putShortMatrix
disp(class(getShortMatrix))
disp(getShortMatrix)
if ~isa(getShortMatrix, 'int16')
	error('getShortMatrix not int16!')
end

disp('getShortMatrixAsList')
if exist('getShortMatrixAsList')
	error('getShortMatrixAsList already defined!')
end
getShortMatrixAsList = putShortMatrixAsList
disp(class(getShortMatrixAsList))
disp(getShortMatrixAsList)
if ~isa(getShortMatrixAsList, 'int16')
	error('getShortMatrixAsList not int16!')
end
