disp('getInteger')
if exists('getInteger') then
	error('getInteger already defined!')
end
getInteger = putInteger
disp(typeof(getInteger))
disp(getInteger)
if typeof(getInteger)~='int32' then
	error('getInteger not int32!')
end

disp('getIntegerVector')
if exists('getIntegerVector') then
	error('getIntegerVector already defined!')
end
getIntegerVector = putIntegerVector
disp(typeof(getIntegerVector))
disp(getIntegerVector)
if typeof(getIntegerVector)~='int32' then
	error('getIntegerVector not int32!')
end

disp('getIntegerVectorAsList')
if exists('getIntegerVectorAsList') then
	error('getIntegerVectorAsList already defined!')
end
getIntegerVectorAsList = putIntegerVectorAsList
disp(typeof(getIntegerVectorAsList))
disp(getIntegerVectorAsList)
if typeof(getIntegerVectorAsList)~='int32' then
	error('getIntegerVectorAsList not int32!')
end

disp('getIntegerMatrix')
if exists('getIntegerMatrix') then
	error('getIntegerMatrix already defined!')
end
getIntegerMatrix = putIntegerMatrix
disp(typeof(getIntegerMatrix))
disp(getIntegerMatrix)
if typeof(getIntegerMatrix)~='int32' then
	error('getIntegerMatrix not int32!')
end

disp('getIntegerMatrixAsList')
if exists('getIntegerMatrixAsList') then
	error('getIntegerMatrixAsList already defined!')
end
getIntegerMatrixAsList = putIntegerMatrixAsList
disp(typeof(getIntegerMatrixAsList))
disp(getIntegerMatrixAsList)
if typeof(getIntegerMatrixAsList)~='int32' then
	error('getIntegerMatrixAsList not int32!')
end
