import net

var sock: Socket = newSocket()

sock.connect("irc.ponychat.net", Port(6667))

sock.send("NICK nimbot\r\n")
sock.send("USER nimbot a a a :bia: a\r\n")

while true:
  var line = TaintedString""

  sock.readLine(line)
  echo(line)
