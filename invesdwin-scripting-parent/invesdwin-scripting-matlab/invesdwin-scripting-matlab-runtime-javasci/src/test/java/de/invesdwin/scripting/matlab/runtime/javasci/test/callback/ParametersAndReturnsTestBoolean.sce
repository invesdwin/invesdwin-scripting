disp('getBoolean')
if exists('getBoolean') then
	error('getBoolean already defined!')
end
getBoolean = callback('getBoolean')
disp(typeof(getBoolean))
disp(getBoolean)
if typeof(getBoolean)~='boolean' then
	error('getBoolean not logical!')
end
callback('setBoolean',getBoolean)

disp('getBooleanVector')
if exists('getBooleanVector') then
	error('getBooleanVector already defined!')
end
getBooleanVector = callback('getBooleanVector')
disp(typeof(getBooleanVector))
disp(getBooleanVector)
if typeof(getBooleanVector)~='boolean' then
	error('getBooleanVector not logical!')
end
callback('setBooleanVector',getBooleanVector)

disp('getBooleanVectorAsList')
if exists('getBooleanVectorAsList') then
	error('getBooleanVectorAsList already defined!')
end
getBooleanVectorAsList = callback('getBooleanVectorAsList')
disp(typeof(getBooleanVectorAsList))
disp(getBooleanVectorAsList)
if typeof(getBooleanVectorAsList)~='boolean' then
	error('getBooleanVectorAsList not logical!')
end
callback('setBooleanVectorAsList',getBooleanVectorAsList)

disp('getBooleanMatrix')
if exists('getBooleanMatrix') then
	error('getBooleanMatrix already defined!')
end
getBooleanMatrix = callback('getBooleanMatrix')
disp(typeof(getBooleanMatrix))
disp(getBooleanMatrix)
if typeof(getBooleanMatrix)~='boolean' then
	error('getBooleanMatrix not logical!')
end
callback('setBooleanMatrix',getBooleanMatrix)

disp('getBooleanMatrixAsList')
if exists('getBooleanMatrixAsList') then
	error('getBooleanMatrixAsList already defined!')
end
getBooleanMatrixAsList = callback('getBooleanMatrixAsList')
disp(typeof(getBooleanMatrixAsList))
disp(getBooleanMatrixAsList)
if typeof(getBooleanMatrixAsList)~='boolean' then
	error('getBooleanMatrixAsList not logical!')
end
callback('setBooleanMatrixAsList',getBooleanMatrixAsList)
