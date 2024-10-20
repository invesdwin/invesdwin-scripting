function installed = callback_packageInstalled(package)
	[user_packages, system_packages] = pkg ("list", package);
	installed = false;
	for k = 1:numel(user_packages)
   		if strcmp(user_packages{k}.name, package)
   			installed = true;
   			return;
   		end
	end
	for k = 1:numel(system_packages)
   		if strcmp(system_packages{k}.name, package)
   			installed = true;
   			return;
   		end
	end
end

if !callback_packageInstalled("instrument-control")
    pkg install -forge instrument-control;
end
pkg load instrument-control;

if !callback_packageInstalled("io")
	pkg install -forge io;
end
pkg load io;

global globalSocketScriptTaskCallbackContextUuid = socketScriptTaskCallbackContextUuid;
global globalSocketScriptTaskCallbackServerHost = socketScriptTaskCallbackServerHost;
global globalSocketScriptTaskCallbackServerPort = socketScriptTaskCallbackServerPort;
global globalSocketScriptTaskCallbackConnected = false;

function callback_createSocket()
	global globalSocketScriptTaskCallbackSocket;
	global globalSocketScriptTaskCallbackServerHost;
	global globalSocketScriptTaskCallbackServerPort;
	global globalSocketScriptTaskCallbackContextUuid;
	global globalSocketScriptTaskCallbackConnected;
    globalSocketScriptTaskCallbackSocket = tcpclient(globalSocketScriptTaskCallbackServerHost, globalSocketScriptTaskCallbackServerPort);
    writeline(globalSocketScriptTaskCallbackSocket, globalSocketScriptTaskCallbackContextUuid);
    globalSocketScriptTaskCallbackConnected = true;
end

function result = callback_invokeSocket(parameters)
	global globalSocketScriptTaskCallbackSocket;
	dims = cellfun(@(x) size(x), parameters, "UniformOutput", false);
    writeline(globalSocketScriptTaskCallbackSocket, strcat(toJSON(dims), ';', toJSON(parameters)));
    returnExpression = strrep(readline(globalSocketScriptTaskCallbackSocket), '__##M@NL@C##__', "\n");
    result = eval(returnExpression);
end

function result = callback(varargin)
	global globalSocketScriptTaskCallbackConnected;
    if !globalSocketScriptTaskCallbackConnected
    	global globalSocketScriptTaskCallbackContextUuid;
        if length(globalSocketScriptTaskCallbackContextUuid) != 0
            callback_createSocket();
        else
            error('IScriptTaskCallback not available');
        end
    end
    result = callback_invokeSocket(varargin);
end

