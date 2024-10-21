disp('getLong')
if exist('getLong')
	error('getLong already defined!')
end
getLong = callback('getLong')
disp(class(getLong))
disp(getLong)
if ~isa(getLong, 'int64')
	error('getLong not int64!')
end
callback('setLong',getLong)

disp('getLongVector')
if exist('getLongVector')
	error('getLongVector already defined!')
end
getLongVector = callback('getLongVector')
disp(class(getLongVector))
disp(getLongVector)
if ~isa(getLongVector, 'int64')
	error('getLongVector not int64!')
end
callback('setLongVector',getLongVector)

disp('getLongVectorAsList')
if exist('getLongVectorAsList')
	error('getLongVectorAsList already defined!')
end
getLongVectorAsList = callback('getLongVectorAsList')
disp(class(getLongVectorAsList))
disp(getLongVectorAsList)
if ~isa(getLongVectorAsList, 'int64')
	error('getLongVectorAsList not int64!')
end
callback('setLongVectorAsList',getLongVectorAsList)

disp('getLongMatrix')
if exist('getLongMatrix')
	error('getLongMatrix already defined!')
end
getLongMatrix = callback('getLongMatrix')
disp(class(getLongMatrix))
disp(getLongMatrix)
if ~isa(getLongMatrix, 'int64')
	error('getLongMatrix not int64!')
end
callback('setLongMatrix',getLongMatrix)

disp('getLongMatrixAsList')
if exist('getLongMatrixAsList')
	error('getLongMatrixAsList already defined!')
end
getLongMatrixAsList = callback('getLongMatrixAsList')
disp(class(getLongMatrixAsList))
disp(getLongMatrixAsList)
if ~isa(getLongMatrixAsList, 'int64')
	error('getLongMatrixAsList not int64!')
end
callback('setLongMatrixAsList',getLongMatrixAsList)
