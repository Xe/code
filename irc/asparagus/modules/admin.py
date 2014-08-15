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

NAME="Administrative commands"
DESC="Things the bot owner would want to do"

import sys

def initModule(cod):
    cod.addBotCommand("DIE", commandDIE, True)
    cod.addBotCommand("LOADMOD",commandLOADMOD, True)

def destroyModule(cod):
    del cod.botcommands["DIE"]
    del cod.botcommands["LOADMOD"]

def commandDIE(cod, line, splitline, source, destination):
    cod.reply(source, destination, "Oh, I am slain.")
    cod.sendLine("QUIT :Requested by %s" % source)
    sys.exit(0)

def commandLOADMOD(cod, line, splitline, source, destination):
    try:
        cod.loadmod(splitline[1])
        cod.reply(source, destination, "Loaded %s" %splitline[1])
    except Exception as e:
        cod.reply(source, destination, "Can't load %s because %s %s" %\
                (splitline[1], type(e), e.message))

