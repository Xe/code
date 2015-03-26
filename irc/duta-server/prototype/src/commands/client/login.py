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

from command import BaseClientCommand

from modes import *

import time

def init_module(parent):
    login_cmd = LoginCommand(parent)

    parent.add_command(login_cmd)

class LoginCommand(BaseClientCommand):
    def __init__(self, parent):
        BaseClientCommand.__init__(self, "login", parent, FLAG_NONE, 3)

    def go(self, line):
        client = line.source

        if len(line.args) != 3:
            client.send_line_from_server("BADPARV LOGIN :Wrong params for LOGIN")
            return

        nick = line.args[0]
        ident = line.args[1]
        real = line.args[2]
        now = int(time.time())

        if not self.parent.legal_nick(nick):
            client.quit("Invalid nick")
            return

        if not self.parent.legal_ident(ident):
            client.quit("Invalid ident")
            return

        client.nick = nick
        client.ident = ident
        client.real = real
        client.uid = self.parent.gen_uid()
        client.ts = now
        client.flags = FLAG_REGISTERED
        client.modes = "+"

        self.parent.add_client(client) # Bursts client to servers

        client.send_line_from_server("WELCOME %s %s :Welcome to the %s Internet Relay Chat network %s!" % (client.uid, self.parent.netname, self.parent.netname, client.nick))
        client.send_line_from_server("IAM %s %s :%s" %\
                (self.parent.sname, self.parent.sid, self.parent.gecos))
        client.send_line_from_server("YOUARE %s %s %s %s :%s" %\
                (client.uid, client.nick, client.ident, client.host, client.real))
        client.send_line_from_server("IHAVE %d %d :clients, channels" %\
                (len(self.parent.clients), len(self.parent.channels)))
        client.send_line_from_server("KINDS LIST :%s" %\
                (" ".join(LISTS.keys())))
        client.send_line_from_server("KINDS STATUS :%s" %\
                (" ".join(STATUSES.values())))
        client.send_line_from_server("KINDS PROP :%s" %\
                (" ".join(PROPS.values())))
        client.send_line_from_server("KINDS SET :%s" %\
                (" ".join(SETS.values())))
        client.send_line_from_server("KINDS UPROP :%s" %\
                (" ".join(UPROPS.values())))
        client.send_line_from_server("PING :%s" % self.parent.sid)

        assert(client.uid in self.parent.clients)

