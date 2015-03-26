# Copyright (C) 2014 Christine Dodrill <shadow.h511@gmail.com> All rights reserved.
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
#
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
#
# 3. This notice may not be removed or altered from any source
#    distribution.
#

import Queue
import socket
import time

from connection import Connection
from modes      import *

class Client:
    """
    Creates a Client object representing a client.
    """

    def __init__(self, nick=None, ident=None, host=None, uid=None,
            realhost=None, real=None, ip=None, ts=None, modes=None,
            staff=False, away=None, account=None):

        """
        Initialize a Client object. Variables there for convenience.
        """

        self.nick = nick
        self.ident = ident
        self.host = host
        self.uid = uid
        self.modes = modes
        self.realhost = realhost
        self.real = real
        self.ip = ip
        self.ts = int(ts) if ts is not None else 0
        self.away = away
        self.account = account if account != None else "*"

        self.props = UPROP_NONE
        self.local = False
        self.metadata = {}
        self.channels = []

        if modes is not None:
            self.add_props_by_string(modes)

        self.staff = self.props & UPROP_IRCOP == UPROP_IRCOP

    def __str__(self):
        return "%s!%s@%s %s %s %s [%s] %s" %\
                (self.nick, self.ident, self.host, self.ip, self.realhost,
                        self.uid, self.real, hex(self.props))

    def gen_EUID(self):
        """
        Creates an EUID string that the uplink will accept
        """

        return "EUID %s 1 %d %s %s %s %s %s %s %s :%s" %\
                (self.nick, self.ts, self.modes, self.ident, self.host, self.ip,
                        self.uid, self.realhost, self.account, self.real)

    def gen_CLIENT(self):
        """
        Creates a CLIENT string that the client will accept
        """

        return "CLIENT %s %s %s %s %s %s %s :%s" %\
                (self.nick, self.ident, self.host, self.uid, self.account,
                        "*" if self.staff else "C", self.ts, self.real)

    def add_props_by_string(self, string):
        direction = True

        for mode in string:
            if mode == "+":
                direction = True
            elif mode == "-":
                direction = False
            elif mode in UMODES:
                self.props = self.props | UMODES[mode] if direction \
                        else self.props & ~(UMODES[mode])

    def send_line_from_server(self, line):
        return self.send_line(":%s %s" % (self.parent.sid, line))

    def quit(self, message="Connection error"):
        self.parent.quit_client(self, message)

    def sendto_adjacent_channels(self, line):
        pass #TODO: FIXME

    def orthogonal(self, other):
        return len(set(self.channels) & set(other.channels)) > 0

class RemoteTS6Client(Client):
    def __init__(self, server, *args):
        self.server = server
        self.local = False

        Client.__init__(self, *args)

    def send_line(self, line):
        self.server.send_line("%s\r\n" % line)

class LocalClient(Client, Connection):
    def __init__(self, socket, parent):
        self.link = socket
        self.flags = FLAG_NONE
        self.tbuf = ""

        Client.__init__(self)
        Connection.__init__(self, socket, parent)

        self.local = True

        self.queue = Queue.Queue()

    def process_line(self, line):
        line.verb = line.verb.lower()
        line.source = self

        try:
            command = self.parent.commands[line.verb]

            command.check(line)

        except KeyError:
            self.send_line(":%s DONTKNOW %s" % (self.parent.sid, line.verb.upper()))

