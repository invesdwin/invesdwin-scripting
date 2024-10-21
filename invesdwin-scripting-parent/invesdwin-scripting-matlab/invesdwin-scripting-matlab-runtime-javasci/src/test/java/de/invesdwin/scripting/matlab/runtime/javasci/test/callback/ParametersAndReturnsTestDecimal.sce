disp('getDecimal')
if exists('getDecimal') then
	error('getDecimal already defined!')
end
getDecimal = callback('getDecimal')
disp(typeof(getDecimal))
disp(getDecimal)
if typeof(getDecimal)~='constant' then
	error('getDecimal not double!')
end
callback('setDecimal',getDecimal)

disp('getDecimalVector')
if exists('getDecimalVector') then
	error('getDecimalVector already defined!')
end
getDecimalVector = callback('getDecimalVector')
disp(typeof(getDecimalVector))
disp(getDecimalVector)
if typeof(getDecimalVector)~='constant' then
	error('getDecimalVector not double!')
end
callback('setDecimalVector',getDecimalVector)

disp('getDecimalVectorAsList')
if exists('getDecimalVectorAsList') then
	error('getDecimalVectorAsList already defined!')
end
getDecimalVectorAsList = callback('getDecimalVectorAsList')
disp(typeof(getDecimalVectorAsList))
disp(getDecimalVectorAsList)
if typeof(getDecimalVectorAsList)~='constant' then
	error('getDecimalVectorAsList not double!')
end
callback('setDecimalVectorAsList',getDecimalVectorAsList)

disp('getDecimalMatrix')
if exists('getDecimalMatrix') then
	error('getDecimalMatrix already defined!')
end
getDecimalMatrix = callback('getDecimalMatrix')
disp(typeof(getDecimalMatrix))
disp(getDecimalMatrix)
if typeof(getDecimalMatrix)~='constant' then
	error('getDecimalMatrix not double!')
end
callback('setDecimalMatrix',getDecimalMatrix)

disp('getDecimalMatrixAsList')
if exists('getDecimalMatrixAsList') then
	error('getDecimalMatrixAsList already defined!')
end
getDecimalMatrixAsList = callback('getDecimalMatrixAsList')
disp(typeof(getDecimalMatrixAsList))
disp(getDecimalMatrixAsList)
if typeof(getDecimalMatrixAsList)~='constant' then
	error('getDecimalMatrixAsList not double!')
end
callback('setDecimalMatrixAsList',getDecimalMatrixAsList)
