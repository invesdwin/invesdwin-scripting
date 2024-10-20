disp('getByte')
if exist('getByte')
	error('getByte already defined!')
end
getByte = callback('getByte')
disp(class(getByte))
disp(getByte)
if ~isa(getByte, 'int8')
	error('getByte not int8!')
end
callback('setByte',getByte)

disp('getByteVector')
if exist('getByteVector')
	error('getByteVector already defined!')
end
getByteVector = callback('getByteVector')
disp(class(getByteVector))
disp(getByteVector)
if ~isa(getByteVector, 'int8')
	error('getByteVector not int8!')
end
callback('setByteVector',getByteVector)

disp('getByteVectorAsList')
if exist('getByteVectorAsList')
	error('getByteVectorAsList already defined!')
end
getByteVectorAsList = callback('getByteVectorAsList')
disp(class(getByteVectorAsList))
disp(getByteVectorAsList)
if ~isa(getByteVectorAsList, 'int8')
	error('getByteVectorAsList not int8!')
end
callback('setByteVectorAsList',getByteVectorAsList)

disp('getByteMatrix')
if exist('getByteMatrix')
	error('getByteMatrix already defined!')
end
getByteMatrix = callback('getByteMatrix')
disp(class(getByteMatrix))
disp(getByteMatrix)
if ~isa(getByteMatrix, 'int8')
	error('getByteMatrix not int8!')
end
callback('setByteMatrix',getByteMatrix)

disp('getByteMatrixAsList')
if exist('getByteMatrixAsList')
	error('getByteMatrixAsList already defined!')
end
getByteMatrixAsList = callback('getByteMatrixAsList')
disp(class(getByteMatrixAsList))
disp(getByteMatrixAsList)
if ~isa(getByteMatrixAsList, 'int8')
	error('getByteMatrixAsList not int8!')
end
callback('setByteMatrixAsList',getByteMatrixAsList)
