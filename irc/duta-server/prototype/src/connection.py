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

from ircmess import IRCLine

from modes import *

import Queue
import select
import socket

# Common flag sets
READ_ONLY = select.POLLIN | select.POLLPRI | select.POLLHUP | select.POLLERR
READ_WRITE = READ_ONLY | select.POLLOUT

class Connection():
    def __init__(self, socket, parent):
        self.link = socket
        self.queue = Queue.Queue()
        self.poller = parent.poller
        self.parent = parent
        self.peername = socket.getpeername()

        self.tbuf = ""

    def fileno(self):
        return self.link.fileno()

    def getpeername(self):
        try:
            return self.link.getpeername()
        except socket.error:
            return self.peername

    def setblocking(self, *args):
        return self.link.setblocking(*args)

    def recv(self, *args):
        ret = self.link.recv(*args)

        if not ret:
            self.close()
            return ""
        else:
            return ret

    def close(self):
        if self.flags & FLAG_REGISTERED == FLAG_REGISTERED:
            self.quit("Connection error: client terminated")
            pass

        print self.getpeername()

        self.parent.poller.unregister(self)
        self.link.close()

        print "Connection for %s closed" % str(self.getpeername())

    def process(self):
        tbuf = self.tbuf + self.recv(2048)
        lines = tbuf.split("\n")

        self.tbuf = lines[-1]
        lines = lines[:-1]

        for line in lines:
            if line[-1] == "\r":
                # Strip possible trailing \r
                line = line[:-1]

            self.process_line(IRCLine(line))

    def process_line(self, line):
        raise NotImplementedError

    def send_line(self, line):
        self.queue.put(str(line))
        self.poller.modify(self, READ_WRITE)

    def _send_line(self):
        try:
            next_msg = self.queue.get_nowait()
        except Queue.Empty:
            # No message waiting, stop trying to check it
            self.poller.modify(self, READ_ONLY)
        else:
            self.link.send(next_msg + "\r\n")

