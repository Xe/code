"""
Copyright (c) 2013, Jessica Williams
All rights reserved.

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
"""

from base_server import ServerConn

from niilib.b36 import base36encode

class P10ServerConn (ServerConn):
    """
    This class translates actions and structures into P10 lines and the inverse.
    """

    def __init__(self, config):
        """
        Initializes what is needed for a P10 connection.
        """

        self.umodes = "+iko"

        self.config = config

        ServerConn.__init__(self, self.config["host"], self.config["port"])

        self.last_uid = 0

    def gen_uid(self):
        "Generates the next available UID"
        uid = base36encode(self.last_uid, alphabet="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz[]")
        self.last_uid = self.last_uid + 1

        return self.numeric + uid

    def send_line_sname(self, line):
        "Send a line to the server prefixed with the server numeric"

        self.send_line("%s %s" % (self.numeric, line))

    def auth(self):
        """
        Sends the needed auth commands to the remote P10 server.
        """

        self.name = self.config["sname"]
        self.numeric = self.config["numeric"]

        passwd = self.config["password"]

        now = int(time.time())

        self.send_line("PASS :%s" % passwd)
        self.send_line("SERVER %s 1 %d %d J10 %s]]] 0 :Gravelir Services" %\
                (self.name, now, now, self.numeric))

    def end_burst(self):
        "Signals the end of burst"

        self.send_line_sname("EB")

    def add_client(self, client):
        """
        Bursts a client to the P10 network.
        """

        now = int(time.time())

        self.send_line("%s N %s 1 %d %s %s +ik ]]]]]] %s :%s" %\
                (self.config["numeric"], client.nick, now, client.user,
                    client.host, client.uid, client.gecos))

    def quit(self, client, reason):
        """
        Sends a quit line to the P10 network.
        """

        self.send_line("%s Q :%s" % (client.uid, reason))

    def change_nick(self, client, nick):
        "Changes a local client's nickname"

        now = int(time.time())
        self.send_line("%s N %s :%d" % (self.numeric, nick, now))

    def _msg_like(self, type, client, target, message):
        "Generic wrapper for PRIVMSG and NOTICE line sending."

        self.send_line("%s %s %s :%s" % (client.uid, type, target, message))

    def privmsg(self, client, target, message):
        "Sends a PRIVMSG to a remote client."

        self._msg_like("P", client, target, message)

    def notice(self, client, target, message):
        "Sends a NOTICE to a remote client."

        self._msg_like("O", client, target, message)

    def join_client(self, client, channel):
        self.send_line("%s J %s %s" % (client.uid, channel.name, channel.ts))

    def kill(self, killer, target, reason):
        "Remotely kills a client off a P10 network."

        self.send_line("%s D %s :%s!%s (%s)" %\
                (killer.uid, target.uid, killer.host, killer.nick, reason))

scons = P10ServerConn

