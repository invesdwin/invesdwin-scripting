disp('getString')
if exist('getString')
	error('getString already defined!')
end
getString = callback('getString')
disp(class(getString))
disp(getString)
if ~isa(getString, 'char')
	error('getString not char!')
end
callback('setString',getString)

disp('getStringWithNull')
if exist('getStringWithNull')
	error('getStringWithNull already defined!')
end
getStringWithNull = callback('getStringWithNull')
disp(class(getStringWithNull))
disp(getStringWithNull)
if ~isa(getStringWithNull, 'double')
	error('getStringWithNull not double!')
end
if ~isnan(getStringWithNull)
	error('getStringWithNull not nan!')
end
callback('setStringWithNull',getStringWithNull)

disp('getStringVector')
if exist('getStringVector')
	error('getStringVector already defined!')
end
getStringVector = callback('getStringVector')
disp(class(getStringVector))
disp(getStringVector)
if ~isa(getStringVector, 'cell')
	error('getStringVector not cell!')
end
callback('setStringVector',getStringVector)


disp('getStringVectorWithNull')
if exist('getStringVectorWithNull')
	error('getStringVectorWithNull already defined!')
end
getStringVectorWithNull = callback('getStringVectorWithNull')
disp(class(getStringVectorWithNull))
disp(getStringVectorWithNull)
if ~isa(getStringVectorWithNull, 'cell')
	error('getStringVectorWithNull not cell!')
end
if ~isempty(getStringVectorWithNull{2})
	error('getStringVectorWithNull{2} not empty!')
end
callback('setStringVectorWithNull',getStringVectorWithNull)

disp('getStringVectorAsList')
if exist('getStringVectorAsList')
	error('getStringVectorAsList already defined!')
end
getStringVectorAsList = callback('getStringVectorAsList')
disp(class(getStringVectorAsList))
disp(getStringVectorAsList)
if ~isa(getStringVectorAsList, 'cell')
	error('getStringVectorAsList not cell!')
end
callback('setStringVectorAsList',getStringVectorAsList)

disp('getStringVectorAsListWithNull')
if exist('getStringVectorAsListWithNull')
	error('getStringVectorAsListWithNull already defined!')
end
getStringVectorAsListWithNull = callback('getStringVectorAsListWithNull')
disp(class(getStringVectorAsListWithNull))
disp(getStringVectorAsListWithNull)
if ~isa(getStringVectorAsListWithNull, 'cell')
	error('getStringVectorAsListWithNull not cell!')
end
if ~isempty(getStringVectorAsListWithNull{2})
	error('getStringVectorAsListWithNull{2} not empty!')
end
callback('setStringVectorAsListWithNull',getStringVectorAsListWithNull)

disp('getStringMatrix')
if exist('getStringMatrix')
	error('getStringMatrix already defined!')
end
getStringMatrix = callback('getStringMatrix')
disp(class(getStringMatrix))
disp(getStringMatrix)
if ~isa(getStringMatrix, 'cell')
	error('getStringMatrix not cell!')
end
callback('setStringMatrix',getStringMatrix)


disp('getStringMatrixWithNull')
if exist('getStringMatrixWithNull')
	error('getStringMatrixWithNull already defined!')
end
getStringMatrixWithNull = callback('getStringMatrixWithNull')
disp(class(getStringMatrixWithNull))
disp(getStringMatrixWithNull)
if ~isa(getStringMatrixWithNull, 'cell')
	error('getStringMatrixWithNull not cell!')
end
if ~isempty(getStringMatrixWithNull{1,1})
	error('getStringMatrixWithNull{1,1} not empty!')
end
if ~isempty(getStringMatrixWithNull{2,2})
	error('getStringMatrixWithNull{2,2} not empty!')
end
if ~isempty(getStringMatrixWithNull{3,3})
	error('getStringMatrixWithNull{3,3} not empty!')
end
callback('setStringMatrixWithNull',getStringMatrixWithNull)

disp('getStringMatrixAsList')
if exist('getStringMatrixAsList')
	error('getStringMatrixAsList already defined!')
end
getStringMatrixAsList = callback('getStringMatrixAsList')
disp(class(getStringMatrixAsList))
disp(getStringMatrixAsList)
if ~isa(getStringMatrixAsList, 'cell')
	error('getStringMatrixAsList not cell!')
end
callback('setStringMatrixAsList',getStringMatrixAsList)

disp('getStringMatrixAsListWithNull')
if exist('getStringMatrixAsListWithNull')
	error('getStringMatrixAsListWithNull already defined!')
end
getStringMatrixAsListWithNull = callback('getStringMatrixAsListWithNull')
disp(class(getStringMatrixAsListWithNull))
disp(getStringMatrixAsListWithNull)
if ~isa(getStringMatrixAsListWithNull, 'cell')
	error('getStringMatrixAsListWithNull not cell!')
end
if ~isempty(getStringMatrixAsListWithNull{1,1})
	error('getStringMatrixAsListWithNull{1,1} not empty!')
end
if ~isempty(getStringMatrixAsListWithNull{2,2})
	error('getStringMatrixAsListWithNull{2,2} not empty!')
end
if ~isempty(getStringMatrixAsListWithNull{3,3})
	error('getStringMatrixAsListWithNull{3,3} not empty!')
end
callback('setStringMatrixAsListWithNull',getStringMatrixAsListWithNull)
