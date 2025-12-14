#disable repl output during initialization by wrapping everything in a begin ... end
begin

	using Sockets
	using Pkg
	using InteractiveUtils
	
	# https://discourse.julialang.org/t/how-to-use-pkg-dependencies-instead-of-pkg-installed/36416/10
	isinstalled(pkg::String) = any(x -> x.name == pkg && x.is_direct_dep, values(Pkg.dependencies()))
	
	if !isinstalled("JSON")
		# redirect stderr to stdout so that stderr parsing on java does not confused falsely in jajub or juliacaller
	    redirect_stderr(stdout) do
	        Pkg.add("JSON")
	    end
	end
	
	using JSON
	
	function writeln(client, str)
		write(client, str)
		write(client, "\n")
	end
	
	"""
	Handles the client connection.
	
	Command list:
		execute: Evaluates the given statement or expression, possible multiple items seperated with semicolon.
	
		get: Returns the value of a given variable in JSON format.
	
		exit: Closes the client connection.
	
		shutdown: Closes the server.
	
	
	Examples:
	
		execute a = 99
	
		get a
	
		(Result is {"a":99})
	
	
		execute using Distributions
	
		execute norm = Normal(0, 1)
	
		execute numbers = rand(norm, 5)
	
		get numbers
	
		(Result is {"numbers":[-1.1734648123994915,-0.5577848419508594,0.5628431386136749,0.42211832659070825,-0.4402667291521316]})
	"""
	function handle_client(server, client, debug)
		while true
			# WORKAOUND: newlines need to be escaped over the wire, unescape here
			__line__ = replace(readline(client), "__##@NL@##__" => "\n")
			if debug
				println(__line__)
			end
			if startswith(__line__, "execute ")
				__command__ = __line__[9:end]
				try
					eval(Meta.parse(__command__))
				catch err
					@error err
				end
			elseif startswith(__line__, "get ")
				__varname__ = __line__[5:end]
				try
					__D__ = Dict(__varname__ => eval(Meta.parse(__varname__)))
					writeln(client, JSON.json(__D__; allownan=true))
				catch err
					@error err
				end
			elseif startswith(__line__, "exit")
				break
			elseif startswith(__line__, "shutdown")
				close(client)
				close(server)
				break
			else
				# sleep 1 ms to not use too much cpu
				sleep(0.001)
			end
		end
		close(client)
	end
	
	
	"""
	Creates a TCP server socket and listens on a given port.
	# Arguments
	- `PORT::Integer`: The port number for the server socket, default is 8000.
	"""
	function serve(PORT=8000, DEBUG=true)
		server = listen(PORT)
		if DEBUG
			println("Listening JuliaCaller on port $PORT")
		end
		while true
			try
				client = accept(server)
				if (@isdefined client)
					handle_client(server, client, DEBUG)
	    		else
					close(client)
				end
			catch err
				close(client)
				close(server)
				writeln("Closed connection and server, exiting")
				break
			end
			# sleep 1 ms to not use too much cpu
			sleep(0.001)
		end
	end

end




