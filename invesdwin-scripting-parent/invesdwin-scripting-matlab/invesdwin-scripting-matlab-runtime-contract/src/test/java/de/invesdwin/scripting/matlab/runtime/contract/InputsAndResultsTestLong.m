disp('getLong')
if exist('getLong')
	error('getLong already defined!')
end
getLong = putLong
disp(class(getLong))
disp(getLong)
if ~isa(getLong, 'int64')
	error('getLong not int64!')
end

disp('getLongVector')
if exist('getLongVector')
	error('getLongVector already defined!')
end
getLongVector = putLongVector
disp(class(getLongVector))
disp(getLongVector)
if ~isa(getLongVector, 'int64')
	error('getLongVector not int64!')
end

disp('getLongVectorAsList')
if exist('getLongVectorAsList')
	error('getLongVectorAsList already defined!')
end
getLongVectorAsList = putLongVectorAsList
disp(class(getLongVectorAsList))
disp(getLongVectorAsList)
if ~isa(getLongVectorAsList, 'int64')
	error('getLongVectorAsList not int64!')
end

disp('getLongMatrix')
if exist('getLongMatrix')
	error('getLongMatrix already defined!')
end
getLongMatrix = putLongMatrix
disp(class(getLongMatrix))
disp(getLongMatrix)
if ~isa(getLongMatrix, 'int64')
	error('getLongMatrix not int64!')
end

disp('getLongMatrixAsList')
if exist('getLongMatrixAsList')
	error('getLongMatrixAsList already defined!')
end
getLongMatrixAsList = putLongMatrixAsList
disp(class(getLongMatrixAsList))
disp(getLongMatrixAsList)
if ~isa(getLongMatrixAsList, 'int64')
	error('getLongMatrixAsList not int64!')
end
