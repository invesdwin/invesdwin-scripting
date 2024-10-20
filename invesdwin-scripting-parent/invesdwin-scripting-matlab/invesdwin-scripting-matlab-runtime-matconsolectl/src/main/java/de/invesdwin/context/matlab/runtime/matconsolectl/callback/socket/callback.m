function result = callback(varargin)
	global globalSocketScriptTaskCallbackConnected;
    if globalSocketScriptTaskCallbackConnected == false
    	global globalSocketScriptTaskCallbackContextUuid;
        if length(globalSocketScriptTaskCallbackContextUuid) ~= 0
            callback_createSocket();
        else
            error('IScriptTaskCallback not available');
        end
    end
    result = callback_invokeSocket(varargin);
end

function callback_createSocket()
	global globalSocketScriptTaskCallbackSocket;
	global globalSocketScriptTaskCallbackServerHost;
	global globalSocketScriptTaskCallbackServerPort;
	global globalSocketScriptTaskCallbackContextUuid;
	global globalSocketScriptTaskCallbackConnected;
    globalSocketScriptTaskCallbackSocket = tcpclient(globalSocketScriptTaskCallbackServerHost, globalSocketScriptTaskCallbackServerPort);
    message = strcat(globalSocketScriptTaskCallbackContextUuid);
    write(globalSocketScriptTaskCallbackSocket, unicode2native(message, 'UTF-8'));
    write(globalSocketScriptTaskCallbackSocket, uint8(newline));
    globalSocketScriptTaskCallbackConnected = true;
end

function result = callback_invokeSocket(parameters)
	global globalSocketScriptTaskCallbackSocket;
	dims = cellfun(@size, parameters, 'UniformOutput', false);
	message = strcat(jsonencode(dims), ';', jsonencode(parameters), newline);
    write(globalSocketScriptTaskCallbackSocket, unicode2native(message, 'UTF-8'));
    write(globalSocketScriptTaskCallbackSocket, uint8(newline));
    prevAvailable = 0;
    while true
    	newAvailable = globalSocketScriptTaskCallbackSocket.BytesAvailable;
    	if prevAvailable > 0 && prevAvailable == newAvailable
    		break;
    	end
    	prevAvailable = newAvailable;
    	pause(0.001);
    end
    returnExpression = strrep(native2unicode(read(globalSocketScriptTaskCallbackSocket), 'UTF-8'), '__##M@NL@C##__', newline);
    % https://de.mathworks.com/matlabcentral/answers/638910-how-can-i-capture-the-numerical-output-from-eval
    returnExpressionLines = split(strtrim(returnExpression), newline);
    returnExpressionLinesLength = length(returnExpressionLines);
    if returnExpressionLinesLength > 1
        returnExpressionExec = strjoin(returnExpressionLines(1:end-1), newline);
        returnExpressionEval = returnExpressionLines{end};
        eval(returnExpressionExec);
        result = eval(returnExpressionEval);
    else
        result = eval(returnExpression);
    end
end
