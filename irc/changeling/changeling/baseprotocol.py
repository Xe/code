import socket

class BaseLink():
    def __init__(self, host, port):
        self.link = socket.socket()

        self.link.connect((host, port))

    def login(self):
        raise NotImplementedError

    def addClient(self):
        raise NotImplementedError

    def privmsg(self):
        raise NotImplementedError

    def notice(self):
         raise NotImplementedError

    def kill(self): #an hero
         raise NotImplementedError

