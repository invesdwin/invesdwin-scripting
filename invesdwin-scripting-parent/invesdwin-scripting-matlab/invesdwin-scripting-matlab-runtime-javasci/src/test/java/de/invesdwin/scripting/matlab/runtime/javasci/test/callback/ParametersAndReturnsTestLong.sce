disp('getLong')
if exists('getLong') then
	error('getLong already defined!')
end
getLong = callback('getLong')
disp(typeof(getLong))
disp(getLong)
if typeof(getLong)~='int64' then
	error('getLong not int64!')
end
callback('setLong',getLong)

disp('getLongVector')
if exists('getLongVector') then
	error('getLongVector already defined!')
end
getLongVector = callback('getLongVector')
disp(typeof(getLongVector))
disp(getLongVector)
if typeof(getLongVector)~='int64' then
	error('getLongVector not int64!')
end
callback('setLongVector',getLongVector)

disp('getLongVectorAsList')
if exists('getLongVectorAsList') then
	error('getLongVectorAsList already defined!')
end
getLongVectorAsList = callback('getLongVectorAsList')
disp(typeof(getLongVectorAsList))
disp(getLongVectorAsList)
if typeof(getLongVectorAsList)~='int64' then
	error('getLongVectorAsList not int64!')
end
callback('setLongVectorAsList',getLongVectorAsList)

disp('getLongMatrix')
if exists('getLongMatrix') then
	error('getLongMatrix already defined!')
end
getLongMatrix = callback('getLongMatrix')
disp(typeof(getLongMatrix))
disp(getLongMatrix)
if typeof(getLongMatrix)~='int64' then
	error('getLongMatrix not int64!')
end
callback('setLongMatrix',getLongMatrix)

disp('getLongMatrixAsList')
if exists('getLongMatrixAsList') then
	error('getLongMatrixAsList already defined!')
end
getLongMatrixAsList = callback('getLongMatrixAsList')
disp(typeof(getLongMatrixAsList))
disp(getLongMatrixAsList)
if typeof(getLongMatrixAsList)~='int64' then
	error('getLongMatrixAsList not int64!')
end
callback('setLongMatrixAsList',getLongMatrixAsList)
