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

import socket
import os
from niilib import log
from select import select

logger = log.Logger("niifpd.log")

POLICY="""<?xml version="1.0"?>
<!DOCTYPE cross-domain-policy SYSTEM "/xml/dtds/cross-domain-policy.dtd">
<cross-domain-policy>
   <site-control permitted-cross-domain-policies="master-only"/>
   <allow-access-from domain="*" to-ports="6667" />
</cross-domain-policy>
"""

server = socket.socket()
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
server.bind(("0.0.0.0", 8430))
server.listen(5)

input = [server]

while 1:
    inputready, outputready, exceptready = select(input,[],[])

    for s in inputready:
        if s == server:
            client, address = server.accept()
            input.append(client)
            logger.log(str(address))

        else:
            data = s.recv(64)
            if data:
                client.send(POLICY)
                client.close()
                input.remove(s)

s.close()

