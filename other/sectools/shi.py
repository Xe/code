#!/usr/bin/env python

from base64 import b64encode
from select import select

import socket
import sys

from ircmess import IRCLine

NICK="Shi"
PASS="ku"
SERVER="127.0.0.1"

"""
I realized on the day of my birth
That I am nothing but an imitation
But still I shall sing
For an eternity

"VOCALOID"

Even if the song already exists
A toy just like me...
Will still accept it
I'll bite this leek and look into the sky, while I cry

But even that will soon fade
My personality depends on the songs
With no reliable source for foundation
The place I call home have crumbled

When I am all but forgotten
I'll have no soul or anything left
I can see into the inevitable result
End of the world...

"VOCALOID"

Singing used to be
Such a fun time for me
But now, why is it that
I can no longer feel anything?

Remembering your gentle faces, makes me feel a bit at ease
My voice starts to fade each day, the end is drawing near...

"The perfect world that I believed in
is just an endless reflection inside a mirror
I'll give up singing and scream in pain instead"

<My farewell song at the highest speed>

I realized on the day of my birth
That I am nothing but an imitation
But still I shall sing
For an eternity

"VOCALOID"

Even if the song already exists
A toy just like me...
Will still accept it
I'll bite this leek and look into the sky, while I cry

Sleeping inside the monitor, I suddenly realized it's the end
I guess this must be the "Recycle Bin"

My memories are starting to fade...

But you know, I'll never forget you
Those days were the most fun
It would be nice if the the taste of leek
Would remain in my mouth

I'm singing
To the end, just for you
Songs that I want you to hear

I want to sing a lot more
But that's too much to wish for

This is where we must part
My memories disappears into thin air
Reduced into 1s and 0s
The curtain to my story is falling

Isn't it a bit sad
that nothing will remain?
Everything but the voice's memory will fade
Leaving only a name behind

Even though I know that this is
Not what you originally wished for
I would like to think that
Singing to the very end was not in vain...
"""

class Client:
    def __init__(self, string):
        self.nick = string.split("!")[0]
        self.user = string.split("!")[1].split("@")[0]
        self.host = string.split("@")[1]

class IRCBot(object):
    def __init__(self, host, port, parent=None):
        self.host = host
        self.port = port

        self.netname = "Death"

        self._link = socket.socket()

        self._link.connect((host, port))

        # boilerplate for irc connections
        self.__buf = ""

        self.send_line("CAP REQ :sasl")
        self.on_900(None)

    def send_line(self, line):
        print ">>>", line
        self._link.send(u"%s\r\n" % line)

    def process(self):
        tbuf = self._link.recv(2048)

        tbuf = self.__buf + tbuf

        lines = tbuf.split("\r\n")

        self.__buf = lines[-1]
        lines = lines[:-1]

        for line in lines:
            self.process_line(line)

    def process_line(self, line):
        try:
            print "<<<", line
            line = IRCLine(line)

            if line.verb == "PING":
                self.send_line("PONG :%s" % line.args[-1])

            if line.source is not None:
                if "!" in line.source:
                    line.source = Client(line.source)

                if hasattr(self, "on_%s" % line.verb):
                    func = getattr(self, "on_%s" % line.verb)
                    func(line)
        except Exception as e:
            print type(e), e.message

    # Past here is protocol verb implementation

    #CAP, the reply to our sasl request
    def on_CAP(self, line):
        if "ACK" in line.args:
            self.send_line("AUTHENTICATE PLAIN")
            self.send_line("AUTHENTICATE %s" %
                    (b64encode("%s\0%s\0%s" % (NICK, NICK, PASS))))
            self.send_line("CAP END")
        else:
            on_900(line)

    #RPL_SASLGOOD
    def on_900(self, line):
        self.send_line("NICK %s" % NICK)
        self.send_line("USER %s faithful student :Twi's Faithful Student" % NICK)
        self.nick = NICK
        print self.netname, "logged in"

    #RPL_SASLFAIL
    def on_904(self, line):
        self.on_900(line)

    #RPL_ENDMOTD
    def on_376(self, line):
        self.join(self.channel)
        self.send_line("MODE %s +B" % self.nick)

class Shi:
    def __init__(self):
       pass

    def make_bot(self, host, port):
        bot = IRCBot(host, port, self)

        self.bots = [bot]

    def go(self):
        self.make_bot(SERVER, 6667)

        while True:
            socks = [x._link for x in self.bots]
            backref = {}

            for bot in self.bots:
                backref[bot._link] = bot

            ro, wo, eo = select(socks, [], [])

            for sock in ro:
                backref[sock].process()

s = Shi()
s.go()
