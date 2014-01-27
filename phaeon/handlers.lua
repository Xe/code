verbs = {}

verbs.PING = function(prefix, args)
	send_line("PONG " .. args)
end

verbs["376"] = function(prefix, args)
	send_line("JOIN " .. CHANNEL)
end

