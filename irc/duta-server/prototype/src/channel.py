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

import client
import time

from modes import * # PEP-8 Violation or not, fuck writing this out by hand
from utils import CaseInsensitiveDict

class ChanUser:
    """
    Wrapper class around Client for channel users
    """

    def __init__(self, client, channel, status=CHFL_PEON):
        self.client = client
        self.status = status
        self.channel = channel

    def add_mode(self, mode):
        add = True

        for char in mode:
            if char == "+":
                add = True
            elif char == "-":
                add = False
            elif char == "v":
                self.status = self.status | CHFL_VOICE if add \
                        else self.status & ~(CHFL_VOICE)
            elif char == "h":
                self.status = self.status | CHFL_HALFOP if add \
                        else self.status & ~(CHFL_HALFOP)
            elif char == "o":
                self.status = self.status | CHFL_CHANOP if add \
                        else self.status & ~(CHFL_CHANOP)

    def add_prefix(self, prefix):
        for char in prefix:
            if char in PREFIXES:
                self.status = self.status | PREFIXES[char]

    def to_prefix(self):
        ret = ""

        if self.status & CHFL_VOICE:
            ret = "+" + ret
        if self.status & CHFL_HALFOP:
            ret = "%" + ret
        if self.status & CHFL_CHANOP:
            ret = "@" + ret

        return ret

    def to_status(self):
        ret = ""

        for status in STATUSES:
            if self.status & status == status:
                ret += STATUSES[status] + " "

class Topic:
    """
    Topic structure
    """

    def __init__(self, topic, client):
        self.topic = topic
        self.setter = client.account if client.account != "*" else client.nick
        self.ts = int(time.time())

class Ban:
    """
    Ban structure
    """

    def __init__(self, mask, ts=None, setter=None, reason=None):
        self.mask = mask
        self.ts = int(time.time()) if ts == None else ts

        if setter is None:
            self.setter = "A.TS6.server"
        else:
            self.setter = setter.account if setter.account != "*" else setter.nick

        self.reason = reason if reason else "No reason given"

class Channel:
    """
    Channel structure
    """

    def __init__(self, name, creator=None, ts=None):
        new = ts == None #if we are making a new channel

        self.name = name
        self.ts = int(ts) if ts != None else int(time.time())

        self.clients = {}
        self.metadata = {}

        self.lists = {
                LIST_BAN:    CaseInsensitiveDict(),
                LIST_QUIET:  CaseInsensitiveDict(),
                LIST_EXCEPT: CaseInsensitiveDict(),
                LIST_INVEX:  CaseInsensitiveDict(),
        }

        self.limit = None
        self.key = None
        self.throttle = None
        self.properties = PROP_NONE

        if new:
            self.topic = Topic("", creator)
            self.metadata["CREATOR"] = creator.account if creator.account != "*" \
                    else creator.nick
        else:
            self.topic = Topic("", client.Client(nick="server"))

    def add_member(self, client, prefix=None):
        uid = client.uid
        cuser = ChanUser(client, self)

        if prefix != None:
            cuser.add_prefix(prefix)

        client.channels.append(cuser)

        self.clients[client.uid] = cuser

    def del_member(self, client, reason=None):
        self.sendto_members(":%s PART %s :%s" %\
             (client.uid, self.name, reason if reason else "Leaving"),
             local=True)

        del self.clients[client.uid]

    def sendto_members(self, message, status=CHFL_PEON, but=None,
            server=False, local=False):
        if not but:
            but = client.Client()

        for uid, chanuser in self.clients.iteritems():
            if chanuser.client.uid == but.uid:
                continue

            if local and not chanuser.client.local:
                continue

            if chanuser.status & status == status:
                if server:
                    chanuser.client.send_line_from_server(message)
                else:
                    chanuser.client.send_line(message)

    def check_banned(self, client):
        return False

    def add_ban(self, ban_type, ban):
        if ban.mask not in self.lists[ban_type]:
            self.lists[ban_type][ban.mask] = ban

        self.sendto_members("LIST %s %s ADD %s %d %s :%s" %\
                (self.name, ban_type, ban.mask, ban.ts, ban.setter, ban.reason),
                server=True, status=CHFL_ANY_OP if ban_type in\
                        [LIST_EXCEPT, LIST_INVEX] else CHFL_PEON, local=True)

    def del_ban(self, ban_type, mask):
        if mask in self.lists[ban_type]:
            del self.lists[ban_type][mask]

    def add_prop_by_letter(self, direction, flag):
        flag = CHANMODES[3][flag]

        self.properties = self.properties | flag if direction \
                else self.properties & ~(flag)

    def prop_string(self):
        ret = ""

        for prop in PROPS:
            if self.properties & prop == prop:
                ret += PROPS[prop] + " "

        return ret[:-1]

    def burst_TS6(self, server):
        status = ""

        for uid, client in self.clients.iteritems():
            status += client.to_prefix() + client.client.uid

        server.send_line(":%s SJOIN %d %s %s :%s" %\
                (server.parent.sid, self.ts, self.name, "+nt", status))

        for key, value in self.metadata.iteritems():
            server.send_line(":%s ENCAP * METADATA SET %s %s :%s" %\
                    (server.parent.sid, self.name, key, value))

    def burst_client(self, client):
        client.send_line_from_server("INFO %s" % self.name)

        status = ""

        for uid, cuser in self.clients.iteritems():
            client.send_line_from_server(cuser.client.gen_CLIENT())
            status += "%s%s " % (cuser.to_prefix(), uid)

        status = status[:-1]
        props = self.prop_string()

        for kind, banlist in self.lists.iteritems():
            for mask, ban in banlist.iteritems():
                client.send_line_from_server("LIST %s %s ADD %s %d %s :%s" %\
                    (self.name, kind, ban.mask, ban.ts, ban.setter, ban.reason))

        client.send_line_from_server("STATUS %s BURST :%s" % (self.name, status))

        if props != "":
            #Send PROP list
            client.send_line_from_server("PROP %s :%s" % (self.name, props))

        if self.topic.topic != "":
            client.send_line_from_server("TOPIC %s %d %s :%s" %\
                    (self.name, self.topic.ts, self.topic.setter,
                        self.topic.topic))

        client.send_line_from_server("END INFO %s :End of information for %s" %\
                (self.name, self.name))

