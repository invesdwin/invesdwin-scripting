disp('getShort')
if exists('getShort') then
	error('getShort already defined!')
end
getShort = callback('getShort')
disp(typeof(getShort))
disp(getShort)
if typeof(getShort)~='int16' then
	error('getShort not int16!')
end
callback('setShort',getShort)

disp('getShortVector')
if exists('getShortVector') then
	error('getShortVector already defined!')
end
getShortVector = callback('getShortVector')
disp(typeof(getShortVector))
disp(getShortVector)
if typeof(getShortVector)~='int16' then
	error('getShortVector not int16!')
end
callback('setShortVector',getShortVector)

disp('getShortVectorAsList')
if exists('getShortVectorAsList') then
	error('getShortVectorAsList already defined!')
end
getShortVectorAsList = callback('getShortVectorAsList')
disp(typeof(getShortVectorAsList))
disp(getShortVectorAsList)
if typeof(getShortVectorAsList)~='int16' then
	error('getShortVectorAsList not int16!')
end
callback('setShortVectorAsList',getShortVectorAsList)

disp('getShortMatrix')
if exists('getShortMatrix')
	error('getShortMatrix already defined!')
end
getShortMatrix = callback('getShortMatrix')
disp(typeof(getShortMatrix))
disp(getShortMatrix)
if typeof(getShortMatrix)~='int16' then
	error('getShortMatrix not int16!')
end
callback('setShortMatrix',getShortMatrix)

disp('getShortMatrixAsList')
if exists('getShortMatrixAsList') then
	error('getShortMatrixAsList already defined!')
end
getShortMatrixAsList = callback('getShortMatrixAsList')
disp(typeof(getShortMatrixAsList))
disp(getShortMatrixAsList)
if typeof(getShortMatrixAsList)~='int16' then
	error('getShortMatrixAsList not int16!')
end
callback('setShortMatrixAsList',getShortMatrixAsList)
