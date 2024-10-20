disp('getLong')
if exists('getLong') then
	error('getLong already defined!')
end
getLong = putLong
disp(typeof(getLong))
disp(getLong)
if typeof(getLong)~='int64' then
	error('getLong not int64!')
end

disp('getLongVector')
if exists('getLongVector') then
	error('getLongVector already defined!')
end
getLongVector = putLongVector
disp(typeof(getLongVector))
disp(getLongVector)
if typeof(getLongVector)~='int64' then
	error('getLongVector not int64!')
end

disp('getLongVectorAsList')
if exists('getLongVectorAsList') then
	error('getLongVectorAsList already defined!')
end
getLongVectorAsList = putLongVectorAsList
disp(typeof(getLongVectorAsList))
disp(getLongVectorAsList)
if typeof(getLongVectorAsList)~='int64' then
	error('getLongVectorAsList not int64!')
end

disp('getLongMatrix')
if exists('getLongMatrix') then
	error('getLongMatrix already defined!')
end
getLongMatrix = putLongMatrix
disp(typeof(getLongMatrix))
disp(getLongMatrix)
if typeof(getLongMatrix)~='int64' then
	error('getLongMatrix not int64!')
end

disp('getLongMatrixAsList')
if exists('getLongMatrixAsList') then
	error('getLongMatrixAsList already defined!')
end
getLongMatrixAsList = putLongMatrixAsList
disp(typeof(getLongMatrixAsList))
disp(getLongMatrixAsList)
if typeof(getLongMatrixAsList)~='int64' then
	error('getLongMatrixAsList not int64!')
end
