disp('getCharacter')
if exist('getCharacter')
	error('getCharacter already defined!')
end
getCharacter = putCharacter
disp(class(getCharacter))
disp(getCharacter)
if ~isa(getCharacter, 'char')
	error('getCharacter not char!')
end

disp('getCharacterVector')
if exist('getCharacterVector')
	error('getCharacterVector already defined!')
end
getCharacterVector = putCharacterVector
disp(class(getCharacterVector))
disp(getCharacterVector)
if ~isa(getCharacterVector, 'cell')
	error('getCharacterVector not cell!')
end

disp('getCharacterVectorAsList')
if exist('getCharacterVectorAsList')
	error('getCharacterVectorAsList already defined!')
end
getCharacterVectorAsList = putCharacterVectorAsList
disp(class(getCharacterVectorAsList))
disp(getCharacterVectorAsList)
if ~isa(getCharacterVectorAsList, 'cell')
	error('getCharacterVectorAsList not cell!')
end

disp('getCharacterMatrix')
if exist('getCharacterMatrix')
	error('getCharacterMatrix already defined!')
end
getCharacterMatrix = putCharacterMatrix
disp(class(getCharacterMatrix))
disp(getCharacterMatrix)
if ~isa(getCharacterMatrix, 'cell')
	error('getCharacterMatrix not cell!')
end

disp('getCharacterMatrixAsList')
if exist('getCharacterMatrixAsList')
	error('getCharacterMatrixAsList already defined!')
end
getCharacterMatrixAsList = putCharacterMatrixAsList
disp(class(getCharacterMatrixAsList))
disp(getCharacterMatrixAsList)
if ~isa(getCharacterMatrixAsList, 'cell')
	error('getCharacterMatrixAsList not cell!')
end