disp('getCharacter')
if exist('getCharacter')
	error('getCharacter already defined!')
end
getCharacter = callback('getCharacter')
disp(class(getCharacter))
disp(getCharacter)
if ~isa(getCharacter, 'char')
	error('getCharacter not char!')
end
callback('setCharacter',getCharacter)

disp('getCharacterVector')
if exist('getCharacterVector')
	error('getCharacterVector already defined!')
end
getCharacterVector = callback('getCharacterVector')
disp(class(getCharacterVector))
disp(getCharacterVector)
if ~isa(getCharacterVector, 'cell')
	error('getCharacterVector not cell!')
end
callback('setCharacterVector',getCharacterVector)

disp('getCharacterVectorAsList')
if exist('getCharacterVectorAsList')
	error('getCharacterVectorAsList already defined!')
end
getCharacterVectorAsList = callback('getCharacterVectorAsList')
disp(class(getCharacterVectorAsList))
disp(getCharacterVectorAsList)
if ~isa(getCharacterVectorAsList, 'cell')
	error('getCharacterVectorAsList not cell!')
end
callback('setCharacterVectorAsList',getCharacterVectorAsList)

disp('getCharacterMatrix')
if exist('getCharacterMatrix')
	error('getCharacterMatrix already defined!')
end
getCharacterMatrix = callback('getCharacterMatrix')
disp(class(getCharacterMatrix))
disp(getCharacterMatrix)
if ~isa(getCharacterMatrix, 'cell')
	error('getCharacterMatrix not cell!')
end
callback('setCharacterMatrix',getCharacterMatrix)

disp('getCharacterMatrixAsList')
if exist('getCharacterMatrixAsList')
	error('getCharacterMatrixAsList already defined!')
end
getCharacterMatrixAsList = callback('getCharacterMatrixAsList')
disp(class(getCharacterMatrixAsList))
disp(getCharacterMatrixAsList)
if ~isa(getCharacterMatrixAsList, 'cell')
	error('getCharacterMatrixAsList not cell!')
end
callback('setCharacterMatrixAsList',getCharacterMatrixAsList)
