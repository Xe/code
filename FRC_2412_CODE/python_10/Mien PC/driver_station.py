#!/usr/bin/python2.7

import pygame, pygtk, gtk, netcomm
from netcomm import bf
from netcomm.dscomm import DriverStationComm

pygame.init()

TEAM_ID = 2412
js_list = []
js_num = 0
js_used_ports = []

class joystick():
    def __init__(self, port):
        for n in js_used_ports:
            if port == n:
                raise RuntimeError("Port already assigned")
        self.port = port
        self.js = pygame.joystick.Joystick(port-1)
        self.js.init()
        global js_list
        js_list.append(self)
        self.axes = []
        self.buttons = bf.bf()

    def get_list_axes(self):
        pygame.event.pump()
        r = []
        for n in range(0,6):
            r.append(self.js.get_axis(n))
        self.axes = r

    def get_buttons(self):
        pygame.event.pump()
        r = bf.bf()
        for n in range(0,11):
            r[n] = self.js.get_button(n)
        self.buttons = r
        
class ds_main:
    def delete_event(self, widget, event, data=None):
        return False
    def destroy(self, widget, data=None):
        gtk.main_quit()
    def 
