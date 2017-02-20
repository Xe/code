import socket
import time

while True:
	s = socket.socket()
	s.connect(("127.0.0.1", 8430))

	s.send("<policy-file-request />\0")
	
	for line in s.makefile("r"):
		print line.strip()
		
	print time.ctime(), "---\n"
	continue
