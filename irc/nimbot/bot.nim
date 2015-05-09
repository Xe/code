import message, net

var sock: Socket = newSocket()

sock.connect("irc.ponychat.net", Port(6667))

sock.send("NICK nimbot\r\n")
sock.send("USER nimbot a a a :bia: a\r\n")

while true:
  var line = TaintedString""
  var m: message.Message

  sock.readLine(line)

  m = line.parseMessage()
  echo ">>> ", $$ m
  echo "    ",  $ m

  case m.verb
  of "PING":
    echo "<<< PONG :" & m.args[0]
    sock.send("PONG :" & m.args[0] & "\r\n")
  of "001":
    echo "<<< JOIN #niichan"
    sock.send("JOIN #niichan\r\n")
