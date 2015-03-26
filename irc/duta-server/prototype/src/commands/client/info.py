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

def init_module(parent):
    info_cmd = InfoCommand(parent)
    myinfo_cmd = MyInfoCommand(parent)

    parent.add_command(info_cmd)
    parent.add_command(myinfo_cmd)

class InfoCommand(BaseClientCommand):
    def __init__(self, parent):
        BaseClientCommand.__init__(self, "info", parent, FLAG_NONE, 1)

    def go(self, line):
        client = line.source
        channame = line.args[0]

        if channame not in self.parent.channels:
            client.send_line_from_server("BADPARV %s %s :No such channel" %\
                    (line.verb.upper(), channame))
            return

        channel = self.parent.channels[channame]
        status = ""

        client.send_line_from_server("INFO %s" % channame)

        for uid, cuser in channel.clients.iteritems():
            client.send_line_from_server(cuser.client.gen_CLIENT())
            status += "%s%s " % (cuser.to_prefix(), uid)

        status = status[:-1]
        props = channel.prop_string()

        for kind, banlist in channel.lists.iteritems():
            for mask, ban in banlist.iteritems():
                client.send_line_from_server("LIST %s %s ADD %s %d %s :%s" %\
                    (channame, kind, ban.mask, ban.ts, ban.setter, ban.reason))

        client.send_line_from_server("STATUS %s BURST :%s" % (channame, status))

        if props != "":
            #Send PROP list
            client.send_line_from_server("PROP %s :%s" % (channame, props))

        client.send_line_from_server("END INFO %s :End of information for %s" %\
                (channame, channame))

    def help(self):
        return "INFO <#channel>\nShows a burst for a channel, including lists"

class MyInfoCommand(BaseClientCommand):
    def __init__(self, parent):
        BaseClientCommand.__init__(self, "myinfo", parent, FLAG_REGISTERED, 0)

    def go(self, line):
        client = line.source
        client.send_line_from_server(client.gen_CLIENT())

    def help(self):
        return "Shows info about yourself"

