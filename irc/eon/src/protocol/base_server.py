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

import socket
import time

from niilib.b36 import base36encode
from niilib import config

class ServerConn(socket.socket):
    """
    Base server connection class. Extends on socket so you can directly call
    select on it.
    """

    def __init__(self, host, port):
        """
        Just sets up the connection.
        """

        socket.socket.__init__(self)
        self.connect((host, port))

    def send_line(self, line):
        """
        Sends a line to the server with the required postfix \\r\\n
        """

        self.send("%s\r\n" % line)

        print ">>>", line

    def load_config(self, fname):
        """
        Loads the configuration file into memeory and parses it
        """

        self.config = config.Config(fname).config

