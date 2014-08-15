from niilib import config
from niilib.message import IRCMessage
from select import select
from bot import *

import socket
import os
import sys
import ssl
import gc
import time
import sqlite3 as lite

VERSION = "0.1"

class Asparagus():
    def __init__(self, configpath):
        self.version = VERSION

        self.link = socket.socket()

        self.channels = []
        self.modules = {}

        self.s2scommands = {"PRIVMSG": [handlePRIVMSG]}
        self.botcommands = {}
        self.ownercommands = {}

        self.socks = [self.link]
        self.sockhandlers = {self.link: self.process}

        self.config = config.Config(configpath).config
        self.db = None

        if self.config["etc"]["fork"]:
            try:
                pid = os.fork()
            except OSError, e:
                raise Exception, "%s [%d]" % (e.strerror, e.errno)

            if (pid == 0):
                os.setsid()
            else:
                os._exit(0)

        if self.config["server"]["ssl"]:
            self.link = ssl.wrap_socket(self.link)

        self.link.connect((self.config["server"]["host"], self.config["server"]["port"]))

        self.sendLine("CAP LS")
        self.sendLine("CAP REQ away-notify")
        self.sendLine("CAP REQ account-notify")
        self.sendLine("CAP END")

        self.sendLine("NICK %s" % self.config["server"]["nick"])
        self.sendLine("USER %s %s bla :%s" %
                (self.config["server"]["user"], self.config["server"]["nick"],
                    self.config["server"]["real"]))

        self.loadmod("znc")

    def sendLine(self, line):
        if self.config["etc"]["debug"]:
            if self.config["etc"]["fork"]:
                pass
            else:
                print ">>>", line

        self.link.send(u"%s\r\n" % line)

    def join(self, channel):
        self.sendLine("JOIN %s" % channel)
        self.channels.append(channel)

    def privmsg(self, target, line):
        self.sendLine("PRIVMSG %s :%s" % (target, line))

    def notice(self, target, line):
        self.sendLine("NOTICE %s :%s" % (target, line))

    def reply(self, source, destination, line):
        if source == destination:
            self.notice(destination, line)
        else:
            self.privmsg(destination, line)

    def addBotCommand(self, name, func, owner=False):
        if owner:
            self.ownercommands[name] = [func]
        else:
            self.botcommands[name] = [func]

    def loadmod(self, modname):
        oldpath = list(sys.path)
        sys.path.insert(0, "modules/")

        self.modules[modname] = __import__(modname)
        self.modules[modname].initModule(self)

        sys.path[:] = oldpath

    def unloadmod(self, modname):
        self.modules[modname].destroyModule(self)
        del self.modules[modname]
        del sys.modules[modname]

        gc.collect()

    def log(self, message, prefix="---"):
        if not self.config["etc"]["fork"]:
            print prefix, str(message).strip()

    def go(self):
        self.buf = ""

        while True:
            inputready, outputready, execeptready = select(self.socks,[],[])

            if len(self.socks) == 0:
                self.log("I am slain")
                sys.exit(0)

            for s in inputready:
                self.sockhandlers[s]([self, s])

    def process(self, args):
        tbuf = self.link.recv(2048)
        tbuf = self.buf + tbuf

        lines = tbuf.split("\r\n")

        self.buf = lines[-1]
        lines = lines[:-1]

        self.processLines(lines)

    def processLines(self, lines):
        for line in lines:
            line = IRCMessage(line)

            if asp.config["etc"]["debug"]:
                try:
                    self.log(line, "<<<")
                except UnicodeEncodeError:
                    pass

            if line.verb == "PING":
                asp.sendLine("PONG %s" % " ".join(line.args))
                continue

            elif line.verb == "ERROR":
                sys.exit(-1)

            elif line.verb == "376":
                asp.sendLine(asp.config["etc"]["authcmd"])
                asp.sendLine("MODE %s +x" % asp.config["server"]["nick"])

                time.sleep(1) #let things settle

                for channel in asp.config["server"]["channels"]:
                    asp.join(channel)

                asp.loadmod("opname")
                asp.loadmod("immature")
                asp.loadmod("mpdclient")
                asp.loadmod("choice")
                asp.loadmod("youtube")
                asp.loadmod("help")
                asp.loadmod("twitchtv")
                asp.loadmod("bf")
                asp.loadmod("fibbonacci")
                asp.loadmod("admin")
                asp.loadmod("weather")
                asp.loadmod("tfw")
                asp.loadmod("reddit")
                asp.loadmod("eqbeats")
                asp.loadmod("danbooru")
                asp.loadmod("derpibooru")
                asp.loadmod("4chan")
                asp.loadmod("btc")
                asp.loadmod("shibe")

                continue

            try:
                for impl in asp.s2scommands[line.verb]:
                    impl(asp, line)
            except Exception as e:
                self.log(e.message, "!!!")


if __name__ == "__main__":
    asp = Asparagus("config.json")
    asp.go()

