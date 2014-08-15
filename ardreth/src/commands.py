from client import *
from ardreth import sendLine

def handleEUID(line, splitline, extparam, source, clients, channels):
    print "New client %s on the network" % splitline[2]
    clients[splitline[9]] = Client(splitline[2], splitline[9], splitline[4], splitline[5], splitline[6], splitline[7], splitline[8], splitline[11], splitline[12][1:])

def handleQUIT(line, splitline, extparam, source, clients, channels):
    print "Quit: %s" % clients[source].nick
    clients.pop(source)

def handleSJOIN(line, splitline, extparam, source, clients, channels):
    try:
        channels[splitline[3]]
    except KeyError as e:
        channels[splitline[3]] = Channel(splitline[3], splitline[2])
    finally:
        uids = extparam.split(" ")
        for uid in uids:
            #Extremely shitty implementation
            idx = 0
            for char in uid:
                if char > '0' and char < '9':
                    break
                else:
                    idx = idx + 1

            client = clients[uid[-9:]]
            prefix = uid[:idx]

            #I warned you this was shitty
            channels[splitline[3]].clientAdd(client, prefix)
            continue

