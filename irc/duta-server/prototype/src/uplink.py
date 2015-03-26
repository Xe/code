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

from channel    import Ban
from client     import RemoteTS6Client
from connection import Connection
from modes      import *

class Uplink(Connection):
    def __init__(self, sock, parent):
        Connection.__init__(self, sock, parent)

        self.bursted = False

        self.parent = parent

    def auth(self):
        self.send_line("PASS %s TS 6 :%s" % \
                (self.parent.password, self.parent.sid))
        self.send_line("CAPAB :QS EX CHW IE KLN KNOCK TB UNKLN CLUSTER ENCAP SERVICES RSFNC SAVE EUID EOPMOD BAN MLOCK")
        self.send_line("SERVER %s 1 :%s" % \
                (self.parent.sname, self.parent.gecos))

        for uid, client in self.parent.clients.iteritems():
            self.send_line(client.gen_EUID())

        for name, channel in self.parent.channels.iteritems():
            channel.burst_TS6(self)

    def burst_client(self, client):
        self.send_line(":%s %s" %(self.parent.sid, client.gen_EUID()))

    def quit_client(self, client, reason):
        self.send_line(":%s QUIT :%s" % (client.uid, reason))

    def send_line(self, line):
        print "[UPLINK] <<< %s" % line
        self.link.send("%s\r\n" % line)

    def process_line(self, line):
        print "[UPLINK] >>> %s" % line

        if line.verb == "PING":
            if not self.bursted:
                self.bursted = True

            self.send_line("PONG :%s" % line.args[-1])

        elif hasattr(self, "on_%s" % line.verb):
            func = getattr(self, "on_%s" % line.verb)
            func(line)

    def on_EUID(self, line):
        client = RemoteTS6Client(self, line.args[0], line.args[4], line.args[5],
                line.args[7], line.args[8], line.args[-1], line.args[6],
                line.args[2], line.args[3], False, None, line.args[-2])

        self.parent.add_client(client)

    def on_MODE(self, line):
        client = self.parent.clients[line.source]

        client.add_props_by_string(line.args[-1])

    def on_SJOIN(self, line):
        chname  = line.args[1]
        modes   = line.args[2]
        clients = (line.args[-1] + " ").split()
        ts      = line.args[0]
        channel = self.parent.add_channel(chname, None, ts)

        for mode in modes[1:]:
            channel.add_prop_by_letter(True, mode)

        for client in clients:
            prefix, client = client[:-9], client[-9:]
            client = self.parent.clients[client]

            channel.add_member(client, prefix)

            for uid, member in channel.clients.iteritems():
                if not client.orthogonal(member.client) and member.client.local:
                    member.client.send_line_from_server(client.gen_CLIENT())

            channel.sendto_members(":%s JOIN %s" %\
                (client.uid, channel.name), local=True)

    def on_JOIN(self, line):
        channel = line.args[1]
        client  = line.source

        channel = self.parent.channels[channel]
        client  = self.parent.clients[client]

        channel.add_member(client)

        for uid, member in channel.clients.iteritems():
            if not client.orthogonal(member.client) and member.client.local:
                member.client.send_line_from_server(client.gen_CLIENT())

        channel.sendto_members(":%s JOIN %s" %\
            (client.uid, channel.name), local=True)

    def on_TMODE(self, line):

        # Okay boys and girls, this is where it gets fun.
        # There are 5 types of channel modes.
        #
        # | Type | Param? | Description          |
        # |:---- |:------ |:-------------------- |
        # |  0   | Yes    | List-like modes      |
        # |  1   | Yes    | Key-like modes       |
        # |  2   | Yes    | argument modes       |
        # |  3   | No     | channel set modes    |
        # |  4   | Yes    | channel prefix modes |

        # :00AAAAAAY TMODE 1383760869 #niichan +yo 6LOAAAAKA 6LOAAAAKA

        channel = line.args[1]
        ts = line.args[0]
        modestring = line.args[2]
        params = line.args[3:]

        channel = self.parent.channels[channel]

        idx = 0
        parc = 0
        set_mode = True

        for mode in modestring:
            if mode == "+":
                set_mode = True

            elif mode == "-":
                set_mode = False

            elif mode in CHANMODES[0]:
                # list-like mode
                if set_mode:
                    ban = Ban(params[parc], None, self.parent.clients[line.source])
                    channel.add_ban(CHANMODES[0][mode], ban)
                else:
                    channel.del_ban(CHANMODES[0][mode], params[parc])

                parc += 1

            elif mode in CHANMODES[1]:
                # Key-like mode
                channel.key = params[parc]
                parc += 1

            elif mode in CHANMODES[2]:
                # SET flags
                pass

            elif mode in CHANMODES[3]:
                channel.add_prop_by_letter(set_mode, mode)

                prop = CHANMODES[3][mode]
                prop = PROPS[prop]

                channel.sendto_members(":%s PROP %s %s%s" %\
                        (line.source, channel.name, "+" if set_mode else "-",
                            prop), local=True)

            elif mode in CHANMODES[4]:
                # STATUS
                string = str("+" if set_mode else "-") + mode

                client = channel.clients[params[parc]]
                client.add_prefix(string)
                parc += 1

                statname = STATUSES[CHANMODES[4][mode]]

                channel.sendto_members(":%s STATUS %s %s %s :%s" %\
                        (line.source, channel.name,
                            "ADD" if set_mode else "REMOVE", client.client.uid,
                            statname), local=True)

    def on_BMASK(self, line):
        # :42F BMASK 1401843105 #test b :fun!*@* test!*@*

        channel = self.parent.channels[line.args[1]]
        ban_type = CHANMODES[0][line.args[2]]

        for mask in line.args[-1].split():
            ban = Ban(mask)
            channel.add_ban(ban_type, ban)

    def on_KILL(self, line):
        target = self.parent.clients[line.args[0]]
        if "Unknown Client" in line.args[-1]:
            return

        try:
            source = self.parent.clients[line.source]
        except KeyError:
            source = None

        self.parent.quit_client(target, line.args[-1])

    def on_QUIT(self, line):
        source = self.parent.clients[line.source]

        self.parent.quit_client(source, line.args[-1])

    def on_PART(self, line):
        source = self.parent.clients[line.source]

        reason = line.args[-1] if len(line.args) > 1 else "Leaving"

        for chanuser in source.channels:
            channel = chanuser.channel

            channel.del_member(source, reason)

    def on_MESSAGE(self, line):
        if not self.bursted:
            return

        if line.args[0][0] == "#":
            channel = self.parent.channels[line.args[0]]

            channel.sendto_members(str(line), local=True)
        else:
            client =  self.parent.clients[line.args[0]]

            client.send_line(str(line))

    def on_PRIVMSG(self, line):
        self.on_MESSAGE(line)

    def on_NOTICE(self, line):
        self.on_MESSAGE(line)

