# Copyright (C) 2014 Christine Dodrill <xena@yolo-swag.com> All rights reserved.
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

import json
import sys

if __name__ == "__main__":
    table = {
            "opers": [],
            "machine": {},
            "admin": {},
            "classes": [],
            "listen": {},
            "auths": [],
            "privsets": [],
            "channel": {},
            "serverhide": {},
            "blacklists": [],
            "aliases": [],
            "general": {},
            "services": [],
            "module": {},
            "autoload": [],
            "cluster": {},
            "shared": {},
            "exempt": {},
            "log": {},
            "connects": [],
    }

    block = ""

    try:
        assert len(sys.argv) == 3
    except:
        print """Usage: %s <path to ircd.conf> <path to json output>
""" % sys.argv[0]
        sys.exit(1)

    lines = []
    finname = sys.argv[1]

    with open(finname, "r") as fin:
        lines = fin.read().split("\n")

    print lines

