MYPORT = 31337

import sys, time
from socket import *

s = socket(AF_INET, SOCK_DGRAM)
s.bind(('', 0))
s.setsockopt(SOL_SOCKET, SO_BROADCAST, 1)

def send(data):
    s.sendto(str(data), ('<broadcast>', MYPORT))
