import socket
from niilib.message import IRCMessage
from select import select
from proj import tasks

class CloudShibe():
    def __init__(self, server):
        self.nick = "CloudShibe"
        self.user = "doge"
        self.server = server

        self.slink = socket.socket()

        self.slink.connect((server, 6667))

        self.sendLine("CAP LS")
        self.sendLine("CAP REQ away-notify")
        self.sendLine("CAP REQ account-notify")
        self.sendLine("CAP END")

        self.sendLine("NICK %s" % self.nick)
        self.sendLine("USER %s %s %s :Wow, such doge, very chat" %\
                (self.user, self.user, self.user))

    def sendLine(self, line):
#        print ">>>", line
        self.slink.send("%s\r\n" % line)

doge = CloudShibe("irc.yolo-swag.com")

socks = [doge.slink]

while 1:
    inputready, outputready, exceptready = select(socks,[],[])

    for s in inputready:
        if s == doge.slink:
            for line in doge.slink.makefile("r"):
                line = line.strip()

#                print "<<<", line

                if line.split()[0] == "PING":
                    #DO NOT FARM OUT PINGS
                    doge.sendLine("PONG %s" % " ".join(line.split()[1:]))
                    continue

                #farm out message parsing to the cloud
                task = tasks.parseLine.delay(line)
                res = task.get(timeout=1)

                if res != None:
                    doge.sendLine(res)

