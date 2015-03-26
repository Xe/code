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
    privmsg_cmd = MessageCommand(parent, "privmsg")
    notice_cmd  = MessageCommand(parent, "notice")

    parent.add_command(privmsg_cmd)
    parent.add_command(notice_cmd)

class MessageCommand(BaseClientCommand):
    def __init__(self, parent, verb):
        BaseClientCommand.__init__(self, verb, parent, FLAG_REGISTERED, 1)

    def go(self, line):
        if line.args[0][0] == "#":
            self.message_channel(line)

        else:
            self.message_user(line)

    def message_channel(self, line):
        client = line.source
        channel = line.args[0]

        if channel not in self.parent.channels:
            client.send_line_from_server("NOSUCH CHANNEL %s" % channel)
            return

        channel = self.parent.channels[channel]

        if channel.check_banned(client):
            client.send_line_from_server("YOUREBANNED %s :Banned from %s (%s)" %\
                (channel.name, channel.name, "Ban set on you :("))
            return

        channel.sendto_members(":%s " % client.uid + str(line),
                but=client, local=True)

        self.parent.sendto_servers(":%s " % client.uid + str(line))

    def message_user(self, line):
        client = line.source
        user = line.args[0]

        if user not in self.parent.clients:
            client.send_line_from_server("NOSUCH CLIENT %s" % user)
            return

        user = self.parent.clients[user]

        user.send_line(":%s " % client.uid + str(line))

    def help(self):
        return "Sends a message to <#channel> if you are allowed to talk there"

