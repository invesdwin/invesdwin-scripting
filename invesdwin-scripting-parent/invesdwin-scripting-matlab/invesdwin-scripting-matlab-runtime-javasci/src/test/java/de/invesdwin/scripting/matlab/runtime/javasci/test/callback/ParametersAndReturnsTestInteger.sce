disp('getInteger')
if exists('getInteger') then
	error('getInteger already defined!')
end
getInteger = callback('getInteger')
disp(typeof(getInteger))
disp(getInteger)
if typeof(getInteger)~='int32' then
	error('getInteger not int32!')
end
callback('setInteger',getInteger)

disp('getIntegerVector')
if exists('getIntegerVector') then
	error('getIntegerVector already defined!')
end
getIntegerVector = callback('getIntegerVector')
disp(typeof(getIntegerVector))
disp(getIntegerVector)
if typeof(getIntegerVector)~='int32' then
	error('getIntegerVector not int32!')
end
callback('setIntegerVector',getIntegerVector)

disp('getIntegerVectorAsList')
if exists('getIntegerVectorAsList') then
	error('getIntegerVectorAsList already defined!')
end
getIntegerVectorAsList = callback('getIntegerVectorAsList')
disp(typeof(getIntegerVectorAsList))
disp(getIntegerVectorAsList)
if typeof(getIntegerVectorAsList)~='int32' then
	error('getIntegerVectorAsList not int32!')
end
callback('setIntegerVectorAsList',getIntegerVectorAsList)

disp('getIntegerMatrix')
if exists('getIntegerMatrix') then
	error('getIntegerMatrix already defined!')
end
getIntegerMatrix = callback('getIntegerMatrix')
disp(typeof(getIntegerMatrix))
disp(getIntegerMatrix)
if typeof(getIntegerMatrix)~='int32' then
	error('getIntegerMatrix not int32!')
end
callback('setIntegerMatrix',getIntegerMatrix)

disp('getIntegerMatrixAsList')
if exists('getIntegerMatrixAsList') then
	error('getIntegerMatrixAsList already defined!')
end
getIntegerMatrixAsList = callback('getIntegerMatrixAsList')
disp(typeof(getIntegerMatrixAsList))
disp(getIntegerMatrixAsList)
if typeof(getIntegerMatrixAsList)~='int32' then
	error('getIntegerMatrixAsList not int32!')
end
callback('setIntegerMatrixAsList',getIntegerMatrixAsList)
