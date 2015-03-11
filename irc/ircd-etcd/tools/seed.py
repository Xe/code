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

# Seed etcd

import etcd
import json
import sys
import time

def is_sequence(arg):
    return (not hasattr(arg, "strip") and
            hasattr(arg, "__getitem__") or
            hasattr(arg, "__iter__"))

client = etcd.Client(port=5001, read_timeout=9001)

table = {}
base = "/ircd"

with open("seed.json", "r") as fin:
    table = json.loads(fin.read())

def write_tree(basepath, item, makedir=True):
    if hasattr(item, "iteritems"): # dict
        if "name" in item:
            if makedir:
                path = "%s/%s" % (basepath, item["name"])
                client.write(path, "", dir=True)
            else:
                path = basepath
        else:
            path = basepath

        print path

        for key, value in item.iteritems():
            write_tree(path+"/"+key, value)

    elif is_sequence(item):
        n = 0

        for value in item:
            path = "%s/%s" % (basepath, n)

            if hasattr(value, "iteritems"):
                if "name" in value:
                    path = basepath
                    pass

            write_tree(path, value)

            n += 1

    else:
        print basepath, item
        client.write(basepath, item)

        #time.sleep(0.25)

for k,v in table.iteritems():
    write_tree(base+"/"+k, table[k], False)

