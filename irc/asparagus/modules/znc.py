"""
Copyright (c) 2013, Christine Dodrill
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

NAME="ZNC authentication handler"
DESC="Sends password to ZNC"

import sys

def initModule(asp):
    asp.s2scommands["464"] = [sendPassword]
    asp.s2scommands["PRIVMSG"].append(handleChannels)

def destroyModule(asp):
    del asp.s2scommands["464"]
    asp.s2scommands["PRIVMSG"].remove(handleChannels)

def sendPassword(asp, line):
    try:
        asp.sendLine("PASS %s" % asp.config["server"]["password"])
    except:
        asp.log("Set the password", "!!!")
        sys.exit(1)

def handleChannels(asp, line):
    if line.args[0] not in asp.channels:
        raise IOError

