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
import os
import time

class Database(object):
    def __init__(self):
        self.keys = {}
        self.path = ""

    def load_from_file(self, path):
        self.path = path
        with open(path, "r") as fin:
            self.keys = dict(self.keys.items() + json.loads(fin.read()).items())

    def nuke(self):
        self.keys = {}

    def commit(self, path=None):
        path = self.path if path is None else path

        os.rename(self.path, self.path + str(time.time()))

        with open(path, "w") as fout:
            fout.write(json.dumps(self.keys, default=lambda o: o.__dict__))

