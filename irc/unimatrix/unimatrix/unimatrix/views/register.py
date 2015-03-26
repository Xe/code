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

from flask import Blueprint, Response, render_template, redirect, request

from unimatrix import db
from unimatrix.models import Server
from unimatrix.names import names

from random import choice

bp = Blueprint("register", __name__)

@bp.route("/")
def get_conf():
    remote = str(request.remote_addr)

    server_info = None

    for server in db.keys["servers"]:
        if server["host"] == remote:
            return "{\"status\": \"error\", \"message\": \"Server already exists\"}", 500
            break

    name = choice(names)

    server_info = Server("%s.yolo-swag.com" % name, "%s the pony!" % name, remote)
    server_info.linksumm = ["genesect.yolo-swag.com"]

    db.keys["servers"].append(server_info.__dict__)
    db.commit()

    return "{\"status\": \"success\", \"message\": \"You are %s.yolo-swag.com\"}" % name, 200

