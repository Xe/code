#!/usr/bin/python

import socket, select
port = 6666
buf = 512

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.bind(('<broadcast>', port))
s.setblocking(0)

def run():
    
    print("Recieving robot Broadcasts, ^C to quit")

    while True:
        try:
            result = select.select([s],[],[])
            msg = result[0][0].recv(buf) 
            print(msg)
        except:
            print(" hit, goodbye")
            break

if __name__ == "__main__":
	run()
