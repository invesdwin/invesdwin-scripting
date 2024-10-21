disp('getInteger')
if exist('getInteger')
	error('getInteger already defined!')
end
getInteger = callback('getInteger')
disp(class(getInteger))
disp(getInteger)
if ~isa(getInteger, 'int32')
	error('getInteger not int32!')
end
callback('setInteger',getInteger)

disp('getIntegerVector')
if exist('getIntegerVector')
	error('getIntegerVector already defined!')
end
getIntegerVector = callback('getIntegerVector')
disp(class(getIntegerVector))
disp(getIntegerVector)
if ~isa(getIntegerVector, 'int32')
	error('getIntegerVector not int32!')
end
callback('setIntegerVector',getIntegerVector)

disp('getIntegerVectorAsList')
if exist('getIntegerVectorAsList')
	error('getIntegerVectorAsList already defined!')
end
getIntegerVectorAsList = callback('getIntegerVectorAsList')
disp(class(getIntegerVectorAsList))
disp(getIntegerVectorAsList)
if ~isa(getIntegerVectorAsList, 'int32')
	error('getIntegerVectorAsList not int32!')
end
callback('setIntegerVectorAsList',getIntegerVectorAsList)

disp('getIntegerMatrix')
if exist('getIntegerMatrix')
	error('getIntegerMatrix already defined!')
end
getIntegerMatrix = callback('getIntegerMatrix')
disp(class(getIntegerMatrix))
disp(getIntegerMatrix)
if ~isa(getIntegerMatrix, 'int32')
	error('getIntegerMatrix not int32!')
end
callback('setIntegerMatrix',getIntegerMatrix)

disp('getIntegerMatrixAsList')
if exist('getIntegerMatrixAsList')
	error('getIntegerMatrixAsList already defined!')
end
getIntegerMatrixAsList = callback('getIntegerMatrixAsList')
disp(class(getIntegerMatrixAsList))
disp(getIntegerMatrixAsList)
if ~isa(getIntegerMatrixAsList, 'int32')
	error('getIntegerMatrixAsList not int32!')
end
callback('setIntegerMatrixAsList',getIntegerMatrixAsList)
