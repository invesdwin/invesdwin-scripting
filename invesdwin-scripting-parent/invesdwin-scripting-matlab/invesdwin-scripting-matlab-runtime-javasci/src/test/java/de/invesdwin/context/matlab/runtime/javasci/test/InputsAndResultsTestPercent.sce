disp('getPercent')
if exists('getPercent') then
	error('getPercent already defined!')
end
getPercent = putPercent
disp(typeof(getPercent))
disp(getPercent)
if typeof(getPercent)~='constant' then
	error('getPercent not double!')
end

disp('getPercentVector')
if exists('getPercentVector') then
	error('getPercentVector already defined!')
end
getPercentVector = putPercentVector
disp(typeof(getPercentVector))
disp(getPercentVector)
if typeof(getPercentVector)~='constant' then
	error('getPercentVector not double!')
end

disp('getPercentVectorAsList')
if exists('getPercentVectorAsList') then
	error('getPercentVectorAsList already defined!')
end
getPercentVectorAsList = putPercentVectorAsList
disp(typeof(getPercentVectorAsList))
disp(getPercentVectorAsList)
if typeof(getPercentVectorAsList)~='constant' then
	error('getPercentVectorAsList not double!')
end

disp('getPercentMatrix')
if exists('getPercentMatrix') then
	error('getPercentMatrix already defined!')
end
getPercentMatrix = putPercentMatrix
disp(typeof(getPercentMatrix))
disp(getPercentMatrix)
if typeof(getPercentMatrix)~='constant' then
	error('getPercentMatrix not double!')
end

disp('getPercentMatrixAsList')
if exists('getPercentMatrixAsList') then
	error('getPercentMatrixAsList already defined!')
end
getPercentMatrixAsList = putPercentMatrixAsList
disp(typeof(getPercentMatrixAsList))
disp(getPercentMatrixAsList)
if typeof(getPercentMatrixAsList)~='constant' then
	error('getPercentMatrixAsList not double!')
end
