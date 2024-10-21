disp('getCharacter')
if exists('getCharacter') then
	error('getCharacter already defined!')
end
getCharacter = putCharacter
disp(typeof(getCharacter))
disp(getCharacter)
if typeof(getCharacter)~='string' then
	error('getCharacter not char!')
end

disp('getCharacterVector')
if exists('getCharacterVector') then
	error('getCharacterVector already defined!')
end
getCharacterVector = putCharacterVector
disp(typeof(getCharacterVector))
disp(getCharacterVector)
if typeof(getCharacterVector)~='string' then
	error('getCharacterVector not cell!')
end

disp('getCharacterVectorAsList')
if exists('getCharacterVectorAsList') then
	error('getCharacterVectorAsList already defined!')
end
getCharacterVectorAsList = putCharacterVectorAsList
disp(typeof(getCharacterVectorAsList))
disp(getCharacterVectorAsList)
if typeof(getCharacterVectorAsList)~='string' then
	error('getCharacterVectorAsList not cell!')
end

disp('getCharacterMatrix')
if exists('getCharacterMatrix') then
	error('getCharacterMatrix already defined!')
end
getCharacterMatrix = putCharacterMatrix
disp(typeof(getCharacterMatrix))
disp(getCharacterMatrix)
if typeof(getCharacterMatrix)~='string' then
	error('getCharacterMatrix not cell!')
end

disp('getCharacterMatrixAsList')
if exists('getCharacterMatrixAsList') then
	error('getCharacterMatrixAsList already defined!')
end
getCharacterMatrixAsList = putCharacterMatrixAsList
disp(typeof(getCharacterMatrixAsList))
disp(getCharacterMatrixAsList)
if typeof(getCharacterMatrixAsList)~='string' then
	error('getCharacterMatrixAsList not cell!')
end