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

from niilib.b36 import *

from utils import CaseInsensitiveDict

import client
import channel

import importlib

class Parent:
    def __init__(self, poller):
        self.backref = {}
        self.clients = {}
        self.modules = {}
        self.channels = CaseInsensitiveDict()
        self.commands = CaseInsensitiveDict()
        self.poller = poller
        self.serverlinks = []

        self.sname = "duta.yolo-swag.com"
        self.gecos = "Today's tech tomorrow"
        self.sid = "420"
        self.password = "dev"
        self.netname = "ShadowNET"
        self.last_uid = 60466176 # 100000 in base 36

    def legal_nick(self, nick):
        #TODO: FIXME
        return True

    def legal_ident(self, ident):
        #TODO: FIXME
        return True

    def gen_uid(self):
        uid = base36encode(self.last_uid)

        self.last_uid = self.last_uid + 1

        return self.sid + uid

    def add_channel(self, chname, creator=None, ts=None):
        chans = [n.lower() for n in self.channels.keys()]
        if chname.lower not in chans:
            chan = channel.Channel(chname, creator, ts)
            self.channels[chname] = chan
            return chan
        else:
            return self.channels[chname]

    def add_client(self, client):
        if client.uid.startswith(self.sid):
            for server in self.serverlinks:
                server.burst_client(client)

        if client.uid not in self.clients.keys():
            self.clients[client.uid] = client
            return client
        else:
            return False

    def add_command(self, impl):
        verb = impl.verb
        if verb not in self.commands:
            self.commands[verb] = impl

    def init_command(self, name):
        #HACK: TODO: replace me!
        mod = importlib.import_module("commands.client.%s" % name)
        mod.init_module(self)

    def init_commands(self):
        for command in ["login", "info", "join", "message"]:
            self.init_command(command)

    def quit_client(self, client, reason):
        for uplink in self.serverlinks:
            uplink.quit_client(client, reason)

        if client.uid.startswith(self.sid):
            client.send_line("ERROR :Closing link: killed: %s" % reason)
            client._send_line()

        clients = set()

        for channel in client.channels:
            clients.update([e.client for e in channel.channel.clients.values() if e.client.local])
            del channel.channel.clients[client.uid]

        for myclient in clients:
            myclient.send_line(":%s QUIT :%s" % (client.uid, reason))

        del self.clients[client.uid]
        del client

    def sendto_servers(self, message):
        for server in self.serverlinks:
            server.send_line(message)

