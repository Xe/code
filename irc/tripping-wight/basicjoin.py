from utils import testUser

u = testUser()

commands = {}

for line in u.link.makefile('r'):
    line = line.strip()

    splitline = line.split()

    u.log(line, "<<<")

    if line[0] != ":":
        if line.split()[0] == "PING":
            u.sendLine("PONG %s" % splitline[1:][0])

    if splitline[1] == "NOTICE":
        if splitline[2] == "Reaper":
            if splitline[3] == ":kick":
                u.sendLine("KICK #test %s" % " ".join(splitline[4:]))
            elif splitline[3] == ":mode":
                modestring = " ".join(splitline[4:])
                u.sendLine("MODE #test %s" % modestring)
            elif splitline[3] == ":part":
                u.sendLine("PART %s" % " ".join(splitline[4:]))
            elif splitline[3] == ":identify":
                u.privmsg("NickServ", "IDENTIFY %s" % u.config["me"]["services_pass"])
            elif splitline[3] == ":join":
                u.sendLine("JOIN %s" % " ".join(splitline[4:]))
            else:
                command = splitline[3][1:].upper()
                params = " ".join(splitline[4:])
                u.sendLine("%s %s" % (command, params))
