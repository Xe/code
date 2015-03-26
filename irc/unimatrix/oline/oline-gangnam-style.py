from jinja2 import Environment, FileSystemLoader

import json
import sys


def gen_CN_line(source, link, config, raw=False):
    link = str(link)

    server = {}

    for net_server in config["servers"]:
        if net_server["name"] == link:
            server = net_server
            break

    ret = dict(
        theirname=link,
        host=server["host"],
        send_password=server["mypass"],
        accept_password=source["mypass"],
        port=6697,
        hub_mask="*",
        raw=raw
    )

    return ret

if __name__ == "__main__":
    env = Environment(loader=FileSystemLoader("."))
    motd = env.get_template('templates/ircd.motd.jinja')

    config = {}

    with open(sys.argv[1] if len(sys.argv) > 1 else "config.json", "r") as fin:
        config = json.loads(fin.read())

    network = config["network"]

    for server in config["servers"]:
        if server["type"] != "noconf":
            template = env.get_template("templates/%s.jinja" % server["type"])
            server["links"] = []

            for link in server["linksumm"]:
                server["links"].append(gen_CN_line(server, link, config,
                                                   link.endswith("int")))

            with open("confs/" + server["name"]+".conf", "w") as fout:
                fout.write(template.render(**locals()))

            with open("confs/" + server["name"]+".motd", "w") as fout:
                fout.write(motd.render(**locals()))
