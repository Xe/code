import threading
import pygame_joy_test

class joyThread(threading.Thread):
    def __init__(self, name="pygame test thread"):
        threading.Thread.__init__(self)
        self.setName(name)
    
    def run(self):
        self.xy=[pygame_joy_test.jx, pygame_joy_test.jy]
    
    def getxy(self):
        return self.xy

n = joyThread()
n.run()


