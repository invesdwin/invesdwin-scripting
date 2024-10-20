disp('getBoolean')
if exist('getBoolean')
	error('getBoolean already defined!')
end
getBoolean = callback('getBoolean')
disp(class(getBoolean))
disp(getBoolean)
if ~isa(getBoolean, 'logical')
	error('getBoolean not logical!')
end
callback('setBoolean',getBoolean)

disp('getBooleanVector')
if exist('getBooleanVector')
	error('getBooleanVector already defined!')
end
getBooleanVector = callback('getBooleanVector')
disp(class(getBooleanVector))
disp(getBooleanVector)
if ~isa(getBooleanVector, 'logical')
	error('getBooleanVector not logical!')
end
callback('setBooleanVector',getBooleanVector)

disp('getBooleanVectorAsList')
if exist('getBooleanVectorAsList')
	error('getBooleanVectorAsList already defined!')
end
getBooleanVectorAsList = callback('getBooleanVectorAsList')
disp(class(getBooleanVectorAsList))
disp(getBooleanVectorAsList)
if ~isa(getBooleanVectorAsList, 'logical')
	error('getBooleanVectorAsList not logical!')
end
callback('setBooleanVectorAsList',getBooleanVectorAsList)

disp('getBooleanMatrix')
if exist('getBooleanMatrix')
	error('getBooleanMatrix already defined!')
end
getBooleanMatrix = callback('getBooleanMatrix')
disp(class(getBooleanMatrix))
disp(getBooleanMatrix)
if ~isa(getBooleanMatrix, 'logical')
	error('getBooleanMatrix not logical!')
end
callback('setBooleanMatrix',getBooleanMatrix)

disp('getBooleanMatrixAsList')
if exist('getBooleanMatrixAsList')
	error('getBooleanMatrixAsList already defined!')
end
getBooleanMatrixAsList = callback('getBooleanMatrixAsList')
disp(class(getBooleanMatrixAsList))
disp(getBooleanMatrixAsList)
if ~isa(getBooleanMatrixAsList, 'logical')
	error('getBooleanMatrixAsList not logical!')
end
callback('setBooleanMatrixAsList',getBooleanMatrixAsList)
