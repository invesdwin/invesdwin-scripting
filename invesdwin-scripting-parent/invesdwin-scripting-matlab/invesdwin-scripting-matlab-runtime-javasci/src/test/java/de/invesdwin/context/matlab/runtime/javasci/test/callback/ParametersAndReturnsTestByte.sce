disp('getByte')
if exists('getByte') then
	error('getByte already defined!')
end
getByte = callback('getByte')
disp(typeof(getByte))
disp(getByte)
if typeof(getByte)~='int8' then
	error('getByte not int8!')
end
callback('setByte',getByte)

disp('getByteVector')
if exists('getByteVector') then
	error('getByteVector already defined!')
end
getByteVector = callback('getByteVector')
disp(typeof(getByteVector))
disp(getByteVector)
if typeof(getByteVector)~='int8' then
	error('getByteVector not int8!')
end
callback('setByteVector',getByteVector)

disp('getByteVectorAsList')
if exists('getByteVectorAsList') then
	error('getByteVectorAsList already defined!')
end
getByteVectorAsList = callback('getByteVectorAsList')
disp(typeof(getByteVectorAsList))
disp(getByteVectorAsList)
if typeof(getByteVectorAsList)~='int8' then
	error('getByteVectorAsList not int8!')
end
callback('setByteVectorAsList',getByteVectorAsList)

disp('getByteMatrix')
if exists('getByteMatrix') then
	error('getByteMatrix already defined!')
end
getByteMatrix = callback('getByteMatrix')
disp(typeof(getByteMatrix))
disp(getByteMatrix)
if typeof(getByteMatrix)~='int8' then
	error('getByteMatrix not int8!')
end
callback('setByteMatrix',getByteMatrix)

disp('getByteMatrixAsList')
if exists('getByteMatrixAsList') then
	error('getByteMatrixAsList already defined!')
end
getByteMatrixAsList = callback('getByteMatrixAsList')
disp(typeof(getByteMatrixAsList))
disp(getByteMatrixAsList)
if typeof(getByteMatrixAsList)~='int8' then
	error('getByteMatrixAsList not int8!')
end
callback('setByteMatrixAsList',getByteMatrixAsList)
