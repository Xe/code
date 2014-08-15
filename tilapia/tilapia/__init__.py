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

from requests import get, post
import json

class TilapiaAPI(object):
    def __init__(self, username, apikey):
        self.username = username
        self.apikey = apikey

        self.auth = (self.username, self.apikey)

    def list_VPS(self):
        vpslist = get("https://manage.tortois.es/vps/list", auth=self.auth).json
        return vpslist["vpslist"]

    def list_signup_VPS(self):
        signuplist = get("https://manage.tortois.es/vps/signup", auth=self.auth).json

    def signup_VPS(self, plan, region):
        payload = {"plan": plan, "region": region}
        headers = {'content-type': 'application/json'}
        return post("https://manage.tortois.es/vps/signup", auth=self.auth,
                data=json.dumps(payload), headers=headers).json

    def get_VPS_by_id(self, id):
        vps = get("https://manage.tortois.es/vps/%d" % id, auth=self.auth).json
        return vps["service"]

    def get_VPS_templates(self, id):
        templates = get("https://manage.tortois.es/vps/%d/deploy" % id, auth=self.auth).json
        return templates["templates"]

    def deploy_VPS_template(self, id, imagename, rootpass, arch):
        payload = {"imagename": imagename, "rootpass": rootpass, "arch": arch}
        headers = {'content-type': 'application/json'}
        return post("https://manage.tortois.es/vps/%d/deploy" % id, auth=self.auth,
                data=json.dumps(payload), headers=headers).json

    def set_VPS_nickname(self, id, nickname):
        payload = {"nickname": nickname}
        headers = {'content-type': 'application/json'}
        return post("https://manage.tortois.es/vps/%d/setnickname" % id,
                auth=self.auth, data=json.dumps(payload), headers=headers).json

    def enable_VPS_monitoring(self, id):
        return get("https://manage.tortois.es/vps/%d/monitoring/enable" % id, auth=self.auth).json

    def disable_VPS_monitoring(self, id):
        return get("https://manage.tortois.es/vps/%d/monitoring/disable" % id, auth=self.auth).json

    def startup(self, id):
        return get("https://manage.tortois.es/vps/%d/create" % id, auth=self.auth).json

    def shutdown(self, id, action="shutdown"):
        if action == "shutdown" or action == "destroy":
            return get("https://manage.tortois.es/vps/%d/shutdown" % id, auth=self.auth).json
        else:
            raise NameError("Must be either \"shutdown\" or \"destroy\"")

    def getstatus(self, id):
        return get("https://manage.tortois.es/vps/%d/status.json" % id, auth=self.auth).json

    def getjobs(seld, id):
        return get("https://manage.tortois.es/vps/%d/jobs.json" % id, auth=self.auth).json

