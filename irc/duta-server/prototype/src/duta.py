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

from ircmess       import IRCLine
from niilib.config import Config
from niilib.log    import Logger

from client     import LocalClient
from connection import *
from parent     import Parent
from uplink     import Uplink

import channel

import Queue
import select
import socket

TIMEOUT = 1000

# We epoll() now
poller = select.epoll()

# Parent "god" object for IRC
parent = Parent(poller)

# Bind the server socket
sock = socket.socket()
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.bind(("0.0.0.0", 9723)) # 972-385
sock.listen(5)

# Create uplink connection, hardcoded for now
uplink = socket.socket()
uplink.connect(("127.0.0.1", 6667))
uplink_obj = Uplink(uplink, parent)
uplink_obj.auth()

# Register the sockets
poller.register(sock,   READ_ONLY)
poller.register(uplink, READ_ONLY)

# Cheat because this is python
parent.backref = { sock.fileno(): sock, uplink.fileno(): uplink }
parent.serverlinks.append(uplink_obj)

# Load commands
parent.init_commands()

# Event loop
while True:
    #Wait for a socket
    print "=== Waiting"
    events = poller.poll(10000)

    for fd, flag in events:
        # backref the socket
        conn = parent.backref[fd]

        if flag & (select.POLLIN | select.POLLPRI):

            if conn is sock:
                connection, address = conn.accept()
                connection = LocalClient(connection, parent)

                print "New connection from %s" % str(connection.getpeername())
                #connection.peer = str(connection.getpeername())
                connection.host = connection.getpeername()[0]
                connection.realhost = connection.host
                connection.ip = connection.host

                connection.setblocking(0)
                parent.backref[connection.fileno()] = connection
                poller.register(connection, READ_ONLY)

            elif conn is uplink:
                uplink_obj.process()
            else: # This is a client
                #TODO: rewrite this

                conn.process()

        elif flag & select.POLLHUP:
            print "[%s] =!= closed connection" % str(conn.getpeername())
            conn.close()

        elif flag & select.POLLOUT:
            # It's safe to send
            if conn == uplink:
                continue

            conn._send_line()
        elif flag & select.POLLERR:
            # WTF
            print "[%s] disconnected." % str(conn.getpeername())
            conn.close()

