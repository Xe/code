import message, net

var sock: Socket = newSocket()

sock.connect("irc.ponychat.net", Port(6667))

sock.send("NICK nimbot\r\n")
sock.send("USER nimbot a a a :bia: a\r\n")

while true:
  var line = TaintedString""
  var m: message.Message

  sock.readLine(line)
  echo(">>> " & line)

  m = line.parseMessage()

  case m.verb
  of "PING":
    sock.send("PONG :" & m.args[0] & "\r\n")
  of "001":
    sock.send("JOIN #niichan\r\n")
