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

from flask import Blueprint, flash, render_template, redirect

from unimatrix.forms import ServerForm
from unimatrix.models import Server
from unimatrix import db

bp = Blueprint("admin", __name__)

@bp.route("/")
def index():
    return render_template("admin/index.html")

@bp.route("/addserver", methods=['GET', 'POST'])
def addserver():
    form = ServerForm()

    if form.validate_on_submit():
        server = Server(form.name.data, form.description.data, form.ipaddress.data)
        server.linksumm.append("genesect.yolo-swag.com")

        db.keys["servers"].append(server.__dict__)
        db.commit()

        flash("Success! Added %s" % repr(server))
        return redirect("/")
    return render_template("admin/addserver.html", **locals())

