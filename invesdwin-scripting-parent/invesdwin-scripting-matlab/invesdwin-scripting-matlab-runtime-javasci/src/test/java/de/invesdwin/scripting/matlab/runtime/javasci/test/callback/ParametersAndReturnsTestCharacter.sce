disp('getCharacter')
if exists('getCharacter') then
	error('getCharacter already defined!')
end
getCharacter = callback('getCharacter')
disp(typeof(getCharacter))
disp(getCharacter)
if typeof(getCharacter)~='string' then
	error('getCharacter not char!')
end
callback('setCharacter',getCharacter)

disp('getCharacterVector')
if exists('getCharacterVector') then
	error('getCharacterVector already defined!')
end
getCharacterVector = callback('getCharacterVector')
disp(typeof(getCharacterVector))
disp(getCharacterVector)
if typeof(getCharacterVector)~='string' then
	error('getCharacterVector not cell!')
end
callback('setCharacterVector',getCharacterVector)

disp('getCharacterVectorAsList')
if exists('getCharacterVectorAsList') then
	error('getCharacterVectorAsList already defined!')
end
getCharacterVectorAsList = callback('getCharacterVectorAsList')
disp(typeof(getCharacterVectorAsList))
disp(getCharacterVectorAsList)
if typeof(getCharacterVectorAsList)~='string' then
	error('getCharacterVectorAsList not cell!')
end
callback('setCharacterVectorAsList',getCharacterVectorAsList)

disp('getCharacterMatrix')
if exists('getCharacterMatrix') then
	error('getCharacterMatrix already defined!')
end
getCharacterMatrix = callback('getCharacterMatrix')
disp(typeof(getCharacterMatrix))
disp(getCharacterMatrix)
if typeof(getCharacterMatrix)~='string' then
	error('getCharacterMatrix not cell!')
end
callback('setCharacterMatrix',getCharacterMatrix)

disp('getCharacterMatrixAsList')
if exists('getCharacterMatrixAsList') then
	error('getCharacterMatrixAsList already defined!')
end
getCharacterMatrixAsList = callback('getCharacterMatrixAsList')
disp(typeof(getCharacterMatrixAsList))
disp(getCharacterMatrixAsList)
if typeof(getCharacterMatrixAsList)~='string' then
	error('getCharacterMatrixAsList not cell!')
end
callback('setCharacterMatrixAsList',getCharacterMatrixAsList)