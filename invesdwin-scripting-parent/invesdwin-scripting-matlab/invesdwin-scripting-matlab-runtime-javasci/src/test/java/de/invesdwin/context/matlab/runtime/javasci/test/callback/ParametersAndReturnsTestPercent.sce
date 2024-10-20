disp('getPercent')
if exists('getPercent') then
	error('getPercent already defined!')
end
getPercent = callback('getPercent')
disp(typeof(getPercent))
disp(getPercent)
if typeof(getPercent)~='constant' then
	error('getPercent not double!')
end
callback('setPercent',getPercent)

disp('getPercentVector')
if exists('getPercentVector') then
	error('getPercentVector already defined!')
end
getPercentVector = callback('getPercentVector')
disp(typeof(getPercentVector))
disp(getPercentVector)
if typeof(getPercentVector)~='constant' then
	error('getPercentVector not double!')
end
callback('setPercentVector',getPercentVector)

disp('getPercentVectorAsList')
if exists('getPercentVectorAsList') then
	error('getPercentVectorAsList already defined!')
end
getPercentVectorAsList = callback('getPercentVectorAsList')
disp(typeof(getPercentVectorAsList))
disp(getPercentVectorAsList)
if typeof(getPercentVectorAsList)~='constant' then
	error('getPercentVectorAsList not double!')
end
callback('setPercentVectorAsList',getPercentVectorAsList)

disp('getPercentMatrix')
if exists('getPercentMatrix') then
	error('getPercentMatrix already defined!')
end
getPercentMatrix = callback('getPercentMatrix')
disp(typeof(getPercentMatrix))
disp(getPercentMatrix)
if typeof(getPercentMatrix)~='constant' then
	error('getPercentMatrix not double!')
end
callback('setPercentMatrix',getPercentMatrix)

disp('getPercentMatrixAsList')
if exists('getPercentMatrixAsList') then
	error('getPercentMatrixAsList already defined!')
end
getPercentMatrixAsList = callback('getPercentMatrixAsList')
disp(typeof(getPercentMatrixAsList))
disp(getPercentMatrixAsList)
if typeof(getPercentMatrixAsList)~='constant' then
	error('getPercentMatrixAsList not double!')
end
callback('setPercentMatrixAsList',getPercentMatrixAsList)
