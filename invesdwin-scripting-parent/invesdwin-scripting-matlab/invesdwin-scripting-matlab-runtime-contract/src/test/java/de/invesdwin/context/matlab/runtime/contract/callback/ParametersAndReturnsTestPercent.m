disp('getPercent')
if exist('getPercent')
	error('getPercent already defined!')
end
getPercent = callback('getPercent')
disp(class(getPercent))
disp(getPercent)
if ~isa(getPercent, 'double')
	error('getPercent not double!')
end
callback('setPercent',getPercent)

disp('getPercentVector')
if exist('getPercentVector')
	error('getPercentVector already defined!')
end
getPercentVector = callback('getPercentVector')
disp(class(getPercentVector))
disp(getPercentVector)
if ~isa(getPercentVector, 'double')
	error('getPercentVector not double!')
end
callback('setPercentVector',getPercentVector)

disp('getPercentVectorAsList')
if exist('getPercentVectorAsList')
	error('getPercentVectorAsList already defined!')
end
getPercentVectorAsList = callback('getPercentVectorAsList')
disp(class(getPercentVectorAsList))
disp(getPercentVectorAsList)
if ~isa(getPercentVectorAsList, 'double')
	error('getPercentVectorAsList not double!')
end
callback('setPercentVectorAsList',getPercentVectorAsList)

disp('getPercentMatrix')
if exist('getPercentMatrix')
	error('getPercentMatrix already defined!')
end
getPercentMatrix = callback('getPercentMatrix')
disp(class(getPercentMatrix))
disp(getPercentMatrix)
if ~isa(getPercentMatrix, 'double')
	error('getPercentMatrix not double!')
end
callback('setPercentMatrix',getPercentMatrix)

disp('getPercentMatrixAsList')
if exist('getPercentMatrixAsList')
	error('getPercentMatrixAsList already defined!')
end
getPercentMatrixAsList = callback('getPercentMatrixAsList')
disp(class(getPercentMatrixAsList))
disp(getPercentMatrixAsList)
if ~isa(getPercentMatrixAsList, 'double')
	error('getPercentMatrixAsList not double!')
end
callback('setPercentMatrixAsList',getPercentMatrixAsList)
