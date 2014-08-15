#!/usr/bin/python

import socket
from client import *
from commands import *

slink = socket.socket()
clients = {}
channels = {}
commands = {}

commands["EUID"] = handleEUID
commands["QUIT"] = handleQUIT
commands["SJOIN"] = handleSJOIN

def sendLine(line):
    print ">>> %s" % line
    slink.send("%s\r\n" % line)

slink.connect(("127.0.0.1", 6667))

sendLine("PASS dev TS 6 :420")
sendLine("CAPAB :QS EX IE KLN UNKLN ENCAP TB SERVICES EUID EOPMOD MLOCK")
sendLine("SERVER ardreth.shadownet.int 1 :Ardreth testing")

clients["420AAAAAB"] = makeService("Ardreth", "ardreth", "ardreth.shadownet.int", "Ardreth services client", "420AAAAAB")
clients["420AAAAAC"] = makeService("Uldtarras", "uldt", "rras", "Uldtarras", "420AAAAAC")

for client in clients:
    sendLine(clients[client].burst())
    sendLine("SJOIN 1380396931 #services + @%s" % client)

channels["#services"] = Channel("#services", "1380396931", True)

sendLine("PONG 45X")

for line in slink.makefile('r'):
    line = line.strip()

    print "<<< " + line
    splitline = line.split()

    if line[0] != ":":
        #Source is implied to be remote server
        if line.split()[0] == "PING":
           sendLine("PONG %s" % splitline[1:][0])
        else:
            print "!!! UNKNOWN COMMAND"
    else:
        source = splitline[0][1:]

        if splitline[1] == "PRIVMSG":
            if splitline[3] == ":clients":
                for value in clients:
                    sendLine(":%s PRIVMSG %s :%s :%s" % ("420AAAAAB", source, value, str(clients[value])))
            elif splitline[3] == ":channels":
                for value in channels:
                    sendLine(":%s PRIVMSG %s :%s :%s" % ("420AAAAAB", source, value, str(channels[value])))

        elif splitline[1] == "NICK":
            clients[source].nick = splitline[2]

        else:
            try:
                commands[splitline[1]](line, splitline, line.split(":")[2], source, clients, channels)
            except KeyError as e:
                print "!!! UNKNOWN COMMAND"

