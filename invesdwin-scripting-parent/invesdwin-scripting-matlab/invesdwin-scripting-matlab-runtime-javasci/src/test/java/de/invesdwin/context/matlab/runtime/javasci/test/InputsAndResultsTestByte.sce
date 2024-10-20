disp('getByte')
if exists('getByte') then
	error('getByte already defined!')
end
getByte = putByte
disp(typeof(getByte))
disp(getByte)
if typeof(getByte)~='int8' then
	error('getByte not int8!')
end

disp('getByteVector')
if exists('getByteVector') then
	error('getByteVector already defined!')
end
getByteVector = putByteVector
disp(typeof(getByteVector))
disp(getByteVector)
if typeof(getByteVector)~='int8' then
	error('getByteVector not int8!')
end

disp('getByteVectorAsList')
if exists('getByteVectorAsList') then
	error('getByteVectorAsList already defined!')
end
getByteVectorAsList = putByteVectorAsList
disp(typeof(getByteVectorAsList))
disp(getByteVectorAsList)
if typeof(getByteVectorAsList)~='int8' then
	error('getByteVectorAsList not int8!')
end

disp('getByteMatrix')
if exists('getByteMatrix') then
	error('getByteMatrix already defined!')
end
getByteMatrix = putByteMatrix
disp(typeof(getByteMatrix))
disp(getByteMatrix)
if typeof(getByteMatrix)~='int8' then
	error('getByteMatrix not int8!')
end

disp('getByteMatrixAsList')
if exists('getByteMatrixAsList') then
	error('getByteMatrixAsList already defined!')
end
getByteMatrixAsList = putByteMatrixAsList
disp(typeof(getByteMatrixAsList))
disp(getByteMatrixAsList)
if typeof(getByteMatrixAsList)~='int8' then
	error('getByteMatrixAsList not int8!')
end
