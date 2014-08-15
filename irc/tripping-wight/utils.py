import socket
from config import Config

class testUser:
    def __init__(self):
        self.config = Config("config.json").config

        self.link = socket.socket()

        self.link.connect((self.config["server"]["host"],
            self.config["server"]["port"]))

        self.channels = []

        self.sendLine("CAP LS")

        for cap in self.config["me"]["caps"]:
            self.sendLine("CAP REQ %s" % cap)

        self.sendLine("CAP END")
        self.sendLine("NICK %s" % self.config["me"]["nick"])
        self.sendLine("USER a a bia: a")

        for channel in self.config["me"]["channels"]:
            self.join(channel)

    def log(self, message, prefix="---"):
        print prefix, message

    def sendLine(self, line):
        self.log(line, ">>>")
        self.link.send("%s\r\n" % line)

    def privmsg(self, target, line):
        self.sendLine("PRIVMSG %s :%s" % (target, line))

    def join(self, channel):
        self.sendLine("JOIN %s" % channel)


