-- Phaeon

socket = require("socket")

require("config")
require("handlers")

-- Helper functions
function send_line(line, ...)
	line = line:format(...)
	print("<<< " .. line)
	sock:send(line .. "\r\n")
end

function get_line()
	return sock:receive("*l")
end

-- Stolen from https://github.com/AlliedEnvy/leroy
function tokenize_line(line)
	local s, e, prefix = line:find('^:(%S+)')
	local s, e, command = line:find('(%S+)', e and e+1 or 1)
	local s, e, rest = line:find('%s+(.*)', e and e+1 or 1)
	return prefix, command, rest
end

-- Define our socket
sock = socket.tcp()

-- Register with the IRC daemon
sock:connect(HOST, PORT)
send_line("NICK " .. NICK)
send_line("USER %s %s %s :%s", USER, USER, USER, NICK)

-- Main loop
while true do
	line, err = get_line()
	if line then
		print(">>> " .. line)
		prefix, command, args = tokenize_line(line)

		if verbs[command] then
			verbs[command](prefix, rest)
		end
	end
end

