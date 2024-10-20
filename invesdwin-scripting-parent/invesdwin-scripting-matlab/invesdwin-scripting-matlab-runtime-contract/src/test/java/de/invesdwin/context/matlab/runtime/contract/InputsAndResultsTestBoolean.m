disp('getBoolean')
if exist('getBoolean')
	error('getBoolean already defined!')
end
getBoolean = putBoolean
disp(class(getBoolean))
disp(getBoolean)
if ~isa(getBoolean, 'logical')
	error('getBoolean not logical!')
end

disp('getBooleanVector')
if exist('getBooleanVector')
	error('getBooleanVector already defined!')
end
getBooleanVector = putBooleanVector
disp(class(getBooleanVector))
disp(getBooleanVector)
if ~isa(getBooleanVector, 'logical')
	error('getBooleanVector not logical!')
end

disp('getBooleanVectorAsList')
if exist('getBooleanVectorAsList')
	error('getBooleanVectorAsList already defined!')
end
getBooleanVectorAsList = putBooleanVectorAsList
disp(class(getBooleanVectorAsList))
disp(getBooleanVectorAsList)
if ~isa(getBooleanVectorAsList, 'logical')
	error('getBooleanVectorAsList not logical!')
end

disp('getBooleanMatrix')
if exist('getBooleanMatrix')
	error('getBooleanMatrix already defined!')
end
getBooleanMatrix = putBooleanMatrix
disp(class(getBooleanMatrix))
disp(getBooleanMatrix)
if ~isa(getBooleanMatrix, 'logical')
	error('getBooleanMatrix not logical!')
end

disp('getBooleanMatrixAsList')
if exist('getBooleanMatrixAsList')
	error('getBooleanMatrixAsList already defined!')
end
getBooleanMatrixAsList = putBooleanMatrixAsList
disp(class(getBooleanMatrixAsList))
disp(getBooleanMatrixAsList)
if ~isa(getBooleanMatrixAsList, 'logical')
	error('getBooleanMatrixAsList not logical!')
end
