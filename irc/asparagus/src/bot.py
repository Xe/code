def handlePRIVMSG(asp, line):
    source = line.source.split("!")[0]
    destination = line.args[0]
    line = line.args[-1]
    splitline = line.split()

    command = ""

    pm = True

    if destination[0] == "#":
        if destination not in asp.channels:
            return

        if line[0] == asp.config["etc"]["prefix"]:
            command = splitline[0].upper()
            command = command[1:]
            pm = False

    else:
        command = command = splitline[0].upper()

    try:
        if source == asp.config["etc"]["owner"]:
           for impl in asp.ownercommands[command]:
                try:
                    if pm:
                        impl(asp, line, splitline, source, source)
                    else:
                        impl(asp, line, splitline, source, destination)
                except Exception as e:
                    if asp.config["etc"]["debug"]:
                        print e.message
        else:
            raise KeyError
    except KeyError as e:
        try:
            for impl in asp.botcommands[command]:
                 try:
                     if pm:
                         impl(asp, line, splitline, source, source)
                     else:
                         impl(asp, line, splitline, source, destination)
                 except Exception as e:
                     if asp.config["etc"]["debug"]:
                         print e.message
        except Exception as e:
            print e.message

