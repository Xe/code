-- Phaeon

socket = require("socket")

-- Configuration
HOST="irc.yolo-swag.com"
PORT=6667
NICK="Phaeon"
USER="phaeon"
CHANNEL="#niichan"

-- Helper functions
function string.split(str, pat)
	-- from http://lua-users.org/wiki/SplitJoin
	local t = {}
	local fpat = "(.-)" .. pat
	local last_end = 1
	local s, e, cap = str:find(fpat, 1)
	while s do
		if s ~= 1 or cap ~= "" then
			table.insert(t,cap)
		end
		last_end = e+1
		s, e, cap = str:find(fpat, last_end)
	end
	if last_end <= #str then
		cap = str:sub(last_end)
		table.insert(t, cap)
	end
	return t
end

function printf(s, ...)
	io.write(s:format(...))
end

function send_line(line, ...)
	line = line:format(...)
	printf("<<< %s\n", line)
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
send_line("JOIN " .. CHANNEL)

-- Main loop
while true do
	line, err = get_line()
	if line then
		printf(">>> %s\n", line)
		prefix, command, args = tokenize_line(line)

		if command == "PING" then
			send_line("PONG %s", args)
		end

		if command == "376" then
			send_line("JOIN " .. CHANNEL)
		end
	end
end

