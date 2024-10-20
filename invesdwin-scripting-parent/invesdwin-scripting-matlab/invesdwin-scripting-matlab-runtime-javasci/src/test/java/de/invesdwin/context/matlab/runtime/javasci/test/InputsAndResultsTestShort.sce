disp('getShort')
if exists('getShort') then
	error('getShort already defined!')
end
getShort = putShort
disp(typeof(getShort))
disp(getShort)
if typeof(getShort)~='int16' then
	error('getShort not int16!')
end

disp('getShortVector')
if exists('getShortVector') then
	error('getShortVector already defined!')
end
getShortVector = putShortVector
disp(typeof(getShortVector))
disp(getShortVector)
if typeof(getShortVector)~='int16' then
	error('getShortVector not int16!')
end

disp('getShortVectorAsList')
if exists('getShortVectorAsList') then
	error('getShortVectorAsList already defined!')
end
getShortVectorAsList = putShortVectorAsList
disp(typeof(getShortVectorAsList))
disp(getShortVectorAsList)
if typeof(getShortVectorAsList)~='int16' then
	error('getShortVectorAsList not int16!')
end

disp('getShortMatrix')
if exists('getShortMatrix')
	error('getShortMatrix already defined!')
end
getShortMatrix = putShortMatrix
disp(typeof(getShortMatrix))
disp(getShortMatrix)
if typeof(getShortMatrix)~='int16' then
	error('getShortMatrix not int16!')
end

disp('getShortMatrixAsList')
if exists('getShortMatrixAsList') then
	error('getShortMatrixAsList already defined!')
end
getShortMatrixAsList = putShortMatrixAsList
disp(typeof(getShortMatrixAsList))
disp(getShortMatrixAsList)
if typeof(getShortMatrixAsList)~='int16' then
	error('getShortMatrixAsList not int16!')
end
