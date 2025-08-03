global globalScriptTaskCallbackContextRequestPartFile;
globalScriptTaskCallbackContextRequestPartFile = scriptTaskCallbackContextRequestPartFile;
global globalScriptTaskCallbackContextRequestFile;
globalScriptTaskCallbackContextRequestFile = scriptTaskCallbackContextRequestFile;
global globalScriptTaskCallbackContextResponseFile;
globalScriptTaskCallbackContextResponseFile = scriptTaskCallbackContextResponseFile;

function result = callback_dims(parameters)
    for i = 1 : length(parameters)
    	try
		    result(i) = size(parameters(i));
		catch
			try
				result(i) = length(parameters(i));
			catch
		    	result(i) = 0;
		    end
		end
    end
endfunction

function result = callback(varargin)
	global globalScriptTaskCallbackContextRequestPartFile;
	global globalScriptTaskCallbackContextRequestFile;
	global globalScriptTaskCallbackContextResponseFile;
    if length(globalScriptTaskCallbackContextRequestPartFile) == 0 || length(globalScriptTaskCallbackContextRequestFile) == 0 || length(globalScriptTaskCallbackContextResponseFile) == 0
        error('IScriptTaskCallback not available');
    end
    dims = callback_dims(varargin);
	message = strcat([toJSON(dims), ';', toJSON(varargin)]);
	requestFd = mopen(globalScriptTaskCallbackContextRequestPartFile, "wt");
    mputstr(message, requestFd);
    mclose(requestFd);
    movefile(globalScriptTaskCallbackContextRequestPartFile, globalScriptTaskCallbackContextRequestFile);
    while ~isfile(globalScriptTaskCallbackContextResponseFile)
    	sleep(1);
    end
    responseLength = fileinfo(globalScriptTaskCallbackContextResponseFile)(1);
    retry = %T;
    while retry
    	try
	    	responseFd = mopen(globalScriptTaskCallbackContextResponseFile, "rt");
		    returnExpression = mgetstr(responseLength, responseFd);
		    mclose(responseFd);
		    mdelete(globalScriptTaskCallbackContextResponseFile);
		    returnExpressionLines = strsplit(returnExpression, ascii(10));
		    returnExpressionLinesLength = size(returnExpressionLines, 'r');
		    if returnExpressionLinesLength > 1
		        returnExpressionExec = strcat(returnExpressionLines(1:returnExpressionLinesLength-1), ascii(10));
		        returnExpressionEval = returnExpressionLines(returnExpressionLinesLength);
		        execstr(returnExpressionExec);
		        result = evstr(returnExpressionEval);
		    else
		        result = evstr(returnExpression);
		    end
		    retry = %F;
		catch
			//windows file lock might still be active
			sleep(1);
		end
    end
endfunction

