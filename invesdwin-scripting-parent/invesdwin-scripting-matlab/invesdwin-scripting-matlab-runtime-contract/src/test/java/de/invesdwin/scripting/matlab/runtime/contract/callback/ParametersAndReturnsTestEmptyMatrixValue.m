% this workaround is needed for both octave and matlab when using callbacks
% we can actually create functions in scripts without separate files: http://de.mathworks.com/help/matlab/matlab_prog/anonymous-functions.html
ismatlabempty = @(x) all(cellfun(@isempty, x));

getBooleanMatrix = callback('getBooleanMatrix')
if ~ismatlabempty(getBooleanMatrix)
	error('getBooleanMatrix not empty!')
end
callback('setBooleanMatrix',getBooleanMatrix)
getBooleanMatrixAsList = callback('getBooleanMatrixAsList')
if ~ismatlabempty(getBooleanMatrixAsList)
	error('getBooleanMatrixAsList not empty!')
end
callback('setBooleanMatrixAsList',getBooleanMatrixAsList)

getByteMatrix = callback('getByteMatrix')
if ~ismatlabempty(getByteMatrix)
	error('getByteMatrix not empty!')
end
callback('setByteMatrix',getByteMatrix)
getByteMatrixAsList = callback('getByteMatrixAsList')
if ~ismatlabempty(getByteMatrixAsList)
	error('getByteMatrixAsList not empty!')
end
callback('setByteMatrixAsList',getByteMatrixAsList)

getCharacterMatrix = callback('getCharacterMatrix')
if ~ismatlabempty(getCharacterMatrix)
	error('getCharacterMatrix not empty!')
end
callback('setCharacterMatrix',getCharacterMatrix)
getCharacterMatrixAsList = callback('getCharacterMatrixAsList')
if ~ismatlabempty(getCharacterMatrixAsList)
	error('getCharacterMatrixAsList not empty!')
end
callback('setCharacterMatrixAsList',getCharacterMatrixAsList)

getDecimalMatrix = callback('getDecimalMatrix')
if ~ismatlabempty(getDecimalMatrix)
	error('getDecimalMatrix not empty!')
end
callback('setDecimalMatrix',getDecimalMatrix)
getDecimalMatrixAsList = callback('getDecimalMatrixAsList')
if ~ismatlabempty(getDecimalMatrixAsList)
	error('getDecimalMatrixAsList not empty!')
end
callback('setDecimalMatrixAsList',getDecimalMatrixAsList)

getDoubleMatrix = callback('getDoubleMatrix')
if ~ismatlabempty(getDoubleMatrix)
	error('getDoubleMatrix not empty!')
end
callback('setDoubleMatrix',getDoubleMatrix)
getDoubleMatrixAsList = callback('getDoubleMatrixAsList')
if ~ismatlabempty(getDoubleMatrixAsList)
	error('getDoubleMatrixAsList not empty!')
end
callback('setDoubleMatrixAsList',getDoubleMatrixAsList)

getFloatMatrix = callback('getFloatMatrix')
if ~ismatlabempty(getFloatMatrix)
	error('getFloatMatrix not empty!')
end
callback('setFloatMatrix',getFloatMatrix)
getFloatMatrixAsList = callback('getFloatMatrixAsList')
if ~ismatlabempty(getFloatMatrixAsList)
	error('getFloatMatrixAsList not empty!')
end
callback('setFloatMatrixAsList',getFloatMatrixAsList)

getIntegerMatrix = callback('getIntegerMatrix')
if ~ismatlabempty(getIntegerMatrix)
	error('getIntegerMatrix not empty!')
end
callback('setIntegerMatrix',getIntegerMatrix)
getIntegerMatrixAsList = callback('getIntegerMatrixAsList')
if ~ismatlabempty(getIntegerMatrixAsList)
	error('getIntegerMatrixAsList not empty!')
end
callback('setIntegerMatrixAsList',getIntegerMatrixAsList)

getLongMatrix = callback('getLongMatrix')
if ~ismatlabempty(getLongMatrix)
	error('getLongMatrix not empty!')
end
callback('setLongMatrix',getLongMatrix)
getLongMatrixAsList = callback('getLongMatrixAsList')
if ~ismatlabempty(getLongMatrixAsList)
	error('getLongMatrixAsList not empty!')
end
callback('setLongMatrixAsList',getLongMatrixAsList)

getPercentMatrix = callback('getPercentMatrix')
if ~ismatlabempty(getPercentMatrix)
	error('getPercentMatrix not empty!')
end
callback('setPercentMatrix',getPercentMatrix)
getPercentMatrixAsList = callback('getPercentMatrixAsList')
if ~ismatlabempty(getPercentMatrixAsList)
	error('getPercentMatrixAsList not empty!')
end
callback('setPercentMatrixAsList',getPercentMatrixAsList)

getShortMatrix = callback('getShortMatrix')
if ~ismatlabempty(getShortMatrix)
	error('getShortMatrix not empty!')
end
callback('setShortMatrix',getShortMatrix)
getShortMatrixAsList = callback('getShortMatrixAsList')
if ~ismatlabempty(getShortMatrixAsList)
	error('getShortMatrixAsList not empty!')
end
callback('setShortMatrixAsList',getShortMatrixAsList)

getStringMatrix = callback('getStringMatrix')
if ~ismatlabempty(getStringMatrix)
	error('getStringMatrix not empty!')
end
callback('setStringMatrix',getStringMatrix)
getStringMatrixAsList = callback('getStringMatrixAsList')
if ~ismatlabempty(getStringMatrixAsList)
	error('getStringMatrixAsList not empty!')
end
callback('setStringMatrixAsList',getStringMatrixAsList)
