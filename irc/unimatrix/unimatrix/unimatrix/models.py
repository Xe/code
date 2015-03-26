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

import string, os

# Trying this here.
# http://stackoverflow.com/a/14620633/753355
class AttrDict(dict):
    def __init__(self, *args, **kwargs):
        super(AttrDict, self).__init__(*args, **kwargs)
        self.__dict__ = self

class Server(AttrDict):
    def __init__(self, name, description, ipaddr):
        self.name = name
        self.description = description
        self.host = ipaddr

        self.linksumm = []
        self.ilines = []
        self.olines = []
        self.hub = False

        self.gen_sid()
        self.gen_password()

    def gen_sid(self):
        hashval = 1
        for char in str(self.name):
            char = ord(char)
            hashval = hashval * (char * (char + 1))

        self.sid = str(hashval)[:3]

    def gen_password(self):
        chars = string.letters + string.digits + '[]'
        assert 256 % len(chars) == 0  # non-biased later modulo
        PWD_LEN = 16
        self.mypass = ''.join(chars[ord(c) % len(chars)] for c in os.urandom(PWD_LEN))

    def __repr__(self):
        return "<Server %s (%s) [%s] <%s>: %s>" % (self.name, self.sid,
                self.host, self.mypass, self.description)
