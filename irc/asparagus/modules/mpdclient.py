"""
Copyright (c) 2013, Sam Dodrill
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

from mpd import MPDClient, ConnectionError

NAME="MPD Client"
DESC="Does simple lookups and status on an mpd server"

def initModule(cod):
    cod.addBotCommand("MPD", commandMPD)

def yoinkMPD(cod):
    mpd = MPDClient(use_unicode=True)
    mpd.timeout = None
    mpd.idletimeout = None
    mpd.connect(cod.config["mpd"]["host"], cod.config["mpd"]["port"])

    return mpd

def destroyModule(cod):
    cod.log("Disconnecting from MPD server", "===")

    del cod.botcommands["MPD"]

def commandMPD(cod, line, splitline, source, destination):
    mpd = yoinkMPD(cod)

    mpd.update()
    cur = mpd.currentsong()

    cod.reply(source, destination, "%s -- %s" % \
                (cur["artist"], cur["title"]))


