disp('getPercent')
if exist('getPercent')
	error('getPercent already defined!')
end
getPercent = putPercent
disp(class(getPercent))
disp(getPercent)
if ~isa(getPercent, 'double')
	error('getPercent not double!')
end

disp('getPercentVector')
if exist('getPercentVector')
	error('getPercentVector already defined!')
end
getPercentVector = putPercentVector
disp(class(getPercentVector))
disp(getPercentVector)
if ~isa(getPercentVector, 'double')
	error('getPercentVector not double!')
end

disp('getPercentVectorAsList')
if exist('getPercentVectorAsList')
	error('getPercentVectorAsList already defined!')
end
getPercentVectorAsList = putPercentVectorAsList
disp(class(getPercentVectorAsList))
disp(getPercentVectorAsList)
if ~isa(getPercentVectorAsList, 'double')
	error('getPercentVectorAsList not double!')
end

disp('getPercentMatrix')
if exist('getPercentMatrix')
	error('getPercentMatrix already defined!')
end
getPercentMatrix = putPercentMatrix
disp(class(getPercentMatrix))
disp(getPercentMatrix)
if ~isa(getPercentMatrix, 'double')
	error('getPercentMatrix not double!')
end

disp('getPercentMatrixAsList')
if exist('getPercentMatrixAsList')
	error('getPercentMatrixAsList already defined!')
end
getPercentMatrixAsList = putPercentMatrixAsList
disp(class(getPercentMatrixAsList))
disp(getPercentMatrixAsList)
if ~isa(getPercentMatrixAsList, 'double')
	error('getPercentMatrixAsList not double!')
end
