# Copyright (c) 2014 Christine Dodrill <shadow.h511@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import config

import ircmess
import socket
import ssl
import sys

torips = []

def get_ip(line):
    if "." not in line.source:
        return None, None

    if line.verb != "NOTICE":
        return None, None

    string = line.args[-1]
    split = string.split()

    if len(split) < 10:
        return None, None

    if not "Client connecting" in string:
        return None, None

    nick = split[5]
    ip = split[7][1:-1]
    print(ip)

    return nick, ip

def sendLine(sock, line):
    print("OU: ", line)
    line = line + "\r\n"
    sock.send(line.encode("utf-8"))

if __name__ == "__main__":
    with open("torlist", "r") as fin:
        for line in fin.readlines():
            torips.append(line.strip())

    s = socket.socket()
    s = ssl.wrap_socket(s)

    s.connect((config.host, config.port))

    sendLine(s, "NICK " + config.nick)
    sendLine(s, "USER " + config.user + " a a :Tor bot killer")

    for line in s.makefile("r"):
        print("IN: ", line.strip())
        line = ircmess.IRCLine(line)

        if line.verb == "PING":
            sendLine(s, "PONG :%s" % line.args[-1])

        if line.verb == "001":
            sendLine(s, "OPER %s %s" % config.opercreds)
            sendLine(s, "JOIN #services")

        if line.verb == "NOTICE":
            nick, ip = get_ip(line)

            if nick is not None:
                if ip in torips:
                    sendLine(s, "NOTICE %s :You are connecting from a Tor node. "
                            "Your connection will be terminated. Please email %s "
                            "for more information." % (nick, config.staffemail))
                    sendLine(s, "KILL %s :Tor node" % nick)
                    sendLine(s, "PRIVMSG #services :Tor user %s from %s killed" %\
                            (nick, ip))
                    sendLine(s, "PRIVMSG OperServ AKILL ADD *@%s !T 3w Tor node" % ip)

        if line.verb == "ERROR":
            sys.exit()
