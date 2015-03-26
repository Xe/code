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

bp = Blueprint("motd", __name__)

@bp.route("/<network>/<hostname>")
def get_motd(network, hostname):
    remote = str(request.remote_addr)

    server_info = None

    for server in db.keys["servers"]:
        if server["host"] == remote:
            server_info = server
            break

    if server_info is None:
        return "", 500

    server = {
        "name": server_info["name"],
        "description": server_info["description"],
    }

    motd = render_template("motd/%s.motd" % network, **locals())
    return Response(motd, mimetype="text/plain")

