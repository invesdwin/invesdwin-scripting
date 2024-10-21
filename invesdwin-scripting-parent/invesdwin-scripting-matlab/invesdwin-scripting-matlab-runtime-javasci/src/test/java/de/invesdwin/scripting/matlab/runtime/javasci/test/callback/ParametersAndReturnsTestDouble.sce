disp('getDouble')
if exists('getDouble') then
	error('getDouble already defined!')
end
getDouble = callback('getDouble')
disp(typeof(getDouble))
disp(getDouble)
if typeof(getDouble)~='constant' then
	error('getDouble not double!')
end
callback('setDouble',getDouble)

disp('getDoubleVector')
if exists('getDoubleVector') then
	error('getDoubleVector already defined!')
end
getDoubleVector = callback('getDoubleVector')
disp(typeof(getDoubleVector))
disp(getDoubleVector)
if typeof(getDoubleVector)~='constant' then
	error('getDoubleVector not double!')
end
callback('setDoubleVector',getDoubleVector)

disp('getDoubleVectorAsList')
if exists('getDoubleVectorAsList') then
	error('getDoubleVectorAsList already defined!')
end
getDoubleVectorAsList = callback('getDoubleVectorAsList')
disp(typeof(getDoubleVectorAsList))
disp(getDoubleVectorAsList)
if typeof(getDoubleVectorAsList)~='constant' then
	error('getDoubleVectorAsList not double!')
end
callback('setDoubleVectorAsList',getDoubleVectorAsList)

disp('getDoubleMatrix')
if exists('getDoubleMatrix') then
	error('getDoubleMatrix already defined!')
end
getDoubleMatrix = callback('getDoubleMatrix')
disp(typeof(getDoubleMatrix))
disp(getDoubleMatrix)
if typeof(getDoubleMatrix)~='constant' then
	error('getDoubleMatrix not double!')
end
callback('setDoubleMatrix',getDoubleMatrix)

disp('getDoubleMatrixAsList')
if exists('getDoubleMatrixAsList') then
	error('getDoubleMatrixAsList already defined!')
end
getDoubleMatrixAsList = callback('getDoubleMatrixAsList')
disp(typeof(getDoubleMatrixAsList))
disp(getDoubleMatrixAsList)
if typeof(getDoubleMatrixAsList)~='constant' then
	error('getDoubleMatrixAsList not double!')
end
callback('setDoubleMatrixAsList',getDoubleMatrixAsList)
