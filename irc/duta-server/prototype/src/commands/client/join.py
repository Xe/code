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

from channel import Channel
from command import BaseClientCommand

from modes import *

def init_module(parent):
    join_cmd = JoinCommand(parent)

    parent.add_command(join_cmd)

class JoinCommand(BaseClientCommand):
    def __init__(self, parent):
        BaseClientCommand.__init__(self, "join", parent, FLAG_REGISTERED, 1)

    def go(self, line):
        client = line.source
        channel = line.args[0]

        new = False

        if channel in self.parent.channels:
            channel = self.parent.channels[line.args[0]]

        else: # New channel
            channel = Channel(line.args[0], client)
            print "making new channel", line.args[0]
            self.parent.channels[channel.name] = channel
            new = True

        if channel.check_banned(client):
            client.send_line_from_server("YOUREBANNED %s :Banned from %s (%s)" %\
                    (channel.name, channel.name, "Ban set on you :("))
            return

        for uid, member in channel.clients.iteritems():
            if not client.orthogonal(member.client) and member.client.local:
                member.client.send_line_from_server(client.gen_CLIENT())

        channel.add_member(client)
        channel.burst_client(client)

        if new:
            channel.clients[client.uid].status = CHFL_CHANOP
            channel.properties = PROP_INTERNAL | PROP_TOPICREST

            for server in self.parent.serverlinks:
                channel.burst_TS6(server)

        else:
            channel.sendto_members(":%s JOIN %s" %\
                    (client.uid, channel.name), local=True)

            self.parent.sendto_servers(":%s JOIN %s %s +" %\
                    (client.uid, channel.ts, channel.name))

    def help(self):
        return "Joins you to <#channel> if you are allowed to join"

