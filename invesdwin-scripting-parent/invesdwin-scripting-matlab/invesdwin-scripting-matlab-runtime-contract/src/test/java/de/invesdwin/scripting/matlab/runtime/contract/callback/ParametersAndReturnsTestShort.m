disp('getShort')
if exist('getShort')
	error('getShort already defined!')
end
getShort = callback('getShort')
disp(class(getShort))
disp(getShort)
if ~isa(getShort, 'int16')
	error('getShort not int16!')
end
callback('setShort',getShort)

disp('getShortVector')
if exist('getShortVector')
	error('getShortVector already defined!')
end
getShortVector = callback('getShortVector')
disp(class(getShortVector))
disp(getShortVector)
if ~isa(getShortVector, 'int16')
	error('getShortVector not int16!')
end
callback('setShortVector',getShortVector)

disp('getShortVectorAsList')
if exist('getShortVectorAsList')
	error('getShortVectorAsList already defined!')
end
getShortVectorAsList = callback('getShortVectorAsList')
disp(class(getShortVectorAsList))
disp(getShortVectorAsList)
if ~isa(getShortVectorAsList, 'int16')
	error('getShortVectorAsList not int16!')
end
callback('setShortVectorAsList',getShortVectorAsList)

disp('getShortMatrix')
if exist('getShortMatrix')
	error('getShortMatrix already defined!')
end
getShortMatrix = callback('getShortMatrix')
disp(class(getShortMatrix))
disp(getShortMatrix)
if ~isa(getShortMatrix, 'int16')
	error('getShortMatrix not int16!')
end
callback('setShortMatrix',getShortMatrix)

disp('getShortMatrixAsList')
if exist('getShortMatrixAsList')
	error('getShortMatrixAsList already defined!')
end
getShortMatrixAsList = callback('getShortMatrixAsList')
disp(class(getShortMatrixAsList))
disp(getShortMatrixAsList)
if ~isa(getShortMatrixAsList, 'int16')
	error('getShortMatrixAsList not int16!')
end
callback('setShortMatrixAsList',getShortMatrixAsList)
