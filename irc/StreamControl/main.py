import autopy
import socket
import win32com.client as comclt
import time
from message import IRCMessage

SERVER = "irc.yolo-swag.com"
CHANNEL = "#pokemon"
NICK = "StreamInput"

wsh = comclt.Dispatch("WScript.Shell")
wsh.AppActivate("VisualBoyAdvance")

server = socket.socket()
server.connect((SERVER, 6667))

server.send("NICK " + NICK + "\r\n")
server.send("USER a a a a a\r\n")

def pressKey(key):
	wsh.AppActivate("VisualBoyAdvance")
	autopy.key.toggle(key, True)
	time.sleep(1.0/16)
	autopy.key.toggle(key, False)

for line in server.makefile():
	message = IRCMessage(line.strip())

	if message.verb == "376":
		server.send("JOIN %s\r\n" % CHANNEL)
		print "Taking commands"

	elif message.verb == "PRIVMSG":
		key = message.args[-1].lower()

		if key == "a":
			pressKey("z")
		elif key == "b":
			pressKey("x")
		elif key == "down":
			pressKey("j")
		elif key == "up":
			pressKey("k")
		elif key == "left":
			pressKey("h")
		elif key == "right":
			pressKey("l")
		elif key == "start":
			pressKey(";")
		elif key == "select":
			pressKey("p")
	
	elif message.verb == "PING":
		server.send("PONG :%s\r\n" % str(message.args))
