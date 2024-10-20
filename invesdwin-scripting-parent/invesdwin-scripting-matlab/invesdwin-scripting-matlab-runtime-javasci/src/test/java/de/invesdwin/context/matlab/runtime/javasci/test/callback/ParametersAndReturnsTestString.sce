disp('getString')
if exists('getString') then
	error('getString already defined!')
end
getString = callback('getString')
disp(typeof(getString))
disp(getString)
if typeof(getString)~='string' then
	error('getString not char!')
end
callback('setString',getString)

disp('getStringWithNull')
if exists('getStringWithNull') then
	error('getStringWithNull already defined!')
end
getStringWithNull = callback('getStringWithNull')
disp(typeof(getStringWithNull))
disp(getStringWithNull)
if typeof(getStringWithNull)==1 then
	error('getStringWithNull not double!')
end
if ~isnan(getStringWithNull) then
	error('getStringWithNull not nan!')
end
callback('setStringWithNull',getStringWithNull)

disp('getStringVector')
if exists('getStringVector') then
	error('getStringVector already defined!')
end
getStringVector = callback('getStringVector')
disp(typeof(getStringVector))
disp(getStringVector)
if typeof(getStringVector)~='string' then
	error('getStringVector not cell!')
end
callback('setStringVector',getStringVector)


disp('getStringVectorWithNull')
if exists('getStringVectorWithNull') then
	error('getStringVectorWithNull already defined!')
end
getStringVectorWithNull = callback('getStringVectorWithNull')
disp(typeof(getStringVectorWithNull))
disp(getStringVectorWithNull)
if typeof(getStringVectorWithNull)~='string' then
	error('getStringVectorWithNull not cell!')
end
if ~isempty(getStringVectorWithNull(2)) then
	error('getStringVectorWithNull(2) not empty!')
end
callback('setStringVectorWithNull',getStringVectorWithNull)

disp('getStringVectorAsList')
if exists('getStringVectorAsList') then
	error('getStringVectorAsList already defined!')
end
getStringVectorAsList = callback('getStringVectorAsList')
disp(typeof(getStringVectorAsList))
disp(getStringVectorAsList)
if typeof(getStringVectorAsList)~='string' then
	error('getStringVectorAsList not cell!')
end
callback('setStringVectorAsList',getStringVectorAsList)

disp('getStringVectorAsListWithNull')
if exists('getStringVectorAsListWithNull') then
	error('getStringVectorAsListWithNull already defined!')
end
getStringVectorAsListWithNull = callback('getStringVectorAsListWithNull')
disp(typeof(getStringVectorAsListWithNull))
disp(getStringVectorAsListWithNull)
if typeof(getStringVectorAsListWithNull)~='string' then
	error('getStringVectorAsListWithNull not cell!')
end
if ~isempty(getStringVectorAsListWithNull(2)) then
	error('getStringVectorAsListWithNull(2) not empty!')
end
callback('setStringVectorAsListWithNull',getStringVectorAsListWithNull)

disp('getStringMatrix')
if exists('getStringMatrix') then
	error('getStringMatrix already defined!')
end
getStringMatrix = callback('getStringMatrix')
disp(typeof(getStringMatrix))
disp(getStringMatrix)
if typeof(getStringMatrix)~='string' then
	error('getStringMatrix not cell!')
end
callback('setStringMatrix',getStringMatrix)


disp('getStringMatrixWithNull')
if exists('getStringMatrixWithNull') then
	error('getStringMatrixWithNull already defined!')
end
getStringMatrixWithNull = callback('getStringMatrixWithNull')
disp(typeof(getStringMatrixWithNull))
disp(getStringMatrixWithNull)
if typeof(getStringMatrixWithNull)~='string' then
	error('getStringMatrixWithNull not cell!')
end
if ~isempty(getStringMatrixWithNull(1,1)) then
	error('getStringMatrixWithNull(1,1) not empty!')
end
if ~isempty(getStringMatrixWithNull(2,2)) then
	error('getStringMatrixWithNull(2,2) not empty!')
end
if ~isempty(getStringMatrixWithNull(3,3)) then
	error('getStringMatrixWithNull(3,3) not empty!')
end
callback('setStringMatrixWithNull',getStringMatrixWithNull)

disp('getStringMatrixAsList')
if exists('getStringMatrixAsList') then
	error('getStringMatrixAsList already defined!')
end
getStringMatrixAsList = callback('getStringMatrixAsList')
disp(typeof(getStringMatrixAsList))
disp(getStringMatrixAsList)
if typeof(getStringMatrixAsList)~='string' then
	error('getStringMatrixAsList not cell!')
end
callback('setStringMatrixAsList',getStringMatrixAsList)

disp('getStringMatrixAsListWithNull')
if exists('getStringMatrixAsListWithNull') then
	error('getStringMatrixAsListWithNull already defined!')
end
getStringMatrixAsListWithNull = callback('getStringMatrixAsListWithNull')
disp(typeof(getStringMatrixAsListWithNull))
disp(getStringMatrixAsListWithNull)
if typeof(getStringMatrixAsListWithNull)~='string' then
	error('getStringMatrixAsListWithNull not cell!')
end
if ~isempty(getStringMatrixAsListWithNull(1,1)) then
	error('getStringMatrixAsListWithNull(1,1) not empty!')
end
if ~isempty(getStringMatrixAsListWithNull(2,2)) then
	error('getStringMatrixAsListWithNull(2,2) not empty!')
end
if ~isempty(getStringMatrixAsListWithNull(3,3)) then
	error('getStringMatrixAsListWithNull(3,3) not empty!')
end
callback('setStringMatrixAsListWithNull',getStringMatrixAsListWithNull)
