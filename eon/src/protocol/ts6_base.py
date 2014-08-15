from base_server import ServerConn
from niilib.b36 import base36encode

import time

class TS6ServerConn (ServerConn):

    def __init__(self, config):
        self.umodes = "+Sio"

        self.config = config

        ServerConn.__init__(self, self.config["host"], self.config["port"])

        self.last_uid = 60466176 # 100000 in base 36

    def gen_uid(self):
        uid = base36encode(self.last_uid)

        self.last_uid = self.last_uid + 1

        return self.numeric + uid

    def send_line_sname(self, line):
        self.send_line(":%s %s" % (self.numeric, line))

    def auth(self):
        self.name = self.config["sname"]
        self.numeric = self.config["numeric"]

        passwd = self.config["password"]

        self.send_line("PASS %s TS 6 :%s" % (passwd, self.numeric))
        self.send_line("CAPAB :QS EX IE KLN UNKLN ENCAP SERVICES EUID EOPMOD")
        self.send_line("SERVER %s 1 :%s" % (self.name, self.config["desc"]))

    def end_burst(self):
        pass

    def add_client(self, client):
        now = int(time.time())

        self.send_line_sname("EUID %s 1 %d %s %s %s 0 %s * * :%s" %\
                (client.nick, now, client.modes, client.user, client.host,
                    client.uid, client.gecos))

    def quit(self, client, reason):
        self.send_line(":%s QUIT :%s" % (client.uid, reason))

    def change_nick(self, client, nick):
        now = int(time.time())

        self.send_line(":%s NICK %s :%d" % (client.uid, nick, now))

    def _msg_like(self, type, client, target, message):
        self.send_line(":%s %s %s :%s" % (client.uid, type, target, message))

    def privmsg(self, client, target, message):
        self._msg_like("PRIVMSG", client, target, message)

    def notice(self, client, target, message):
        self._msg_like("NOTICE", client, target, message)

    def join_client(self, client, channel):
        self.send_line("SJOIN %s %s + %s" %\
                (channel.ts, channel.name, client.uid))

    def kill(self, killer, target, reason):
        self.send_line(":%s KILL %s :spacing %s" %\
                (killer.uid, target.uid, reason))

scons = TS6ServerConn

