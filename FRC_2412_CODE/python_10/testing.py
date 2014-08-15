#This file should emulate enough of wpilib to make it usable for testing
#TODO: use pygame to get joystick inputs from actual joysticks

import pygame
import pygame.joystick
pygame.init()
print str(pygame.joystick.get_count()) + " Joysticks attatched"
        
def limit(n):
    if n > 1.0:
	return 1.0
    elif n < -1.0:
	return -1.0
    else:
	return n

class Victor:
    def __init__(self, port):
        self.port = port
        print "Virtual Victor set up on port " + str(port)
        self.speed = float()
    
    def Set(self, speed):
        """***FOR TESTING PURPOSES ONLY***
       This "sets" the virtual Victor to any arbitrary speed"""
        speed = limit(speed)
        print "on port " + str(self.port) + " the speed is now " + str(speed)
        self.speed = speed
    
    def Get(self):
        """***FOR TESTING PURPOSES ONLY***
       This emulates the wpilib.Victor.Get function"""
        return self.speed
        
class Joystick_old:
    def __init__(self, port):
        self.port = port
        print "New joystick set up at port "+str(port)
    
    def Get(self):
        return 0.5

class Joystick:
    def __init__(self, port):
        self.port = port -1
        self.joy = pygame.joystick.Joystick(port-1)
        print "made new pygame joystick on port " + str(port)
    
    def GetRawAxis(n):
        return joy.get_axis(n)
    
    def GetRawButton(n):
        return joy.get_button(n)

def speedReport(list_obj):
    """Use this for the driver object"""
    for n in list_obj:
        n.Get()
        
def IsDisabled():
    return False   

def IsAutonomous():
    return False

def IsOperatorControl():
    return True
    
def IsEnabled():
    return True

class dummyDog:
    def SetEnabled(self, var):
        pass
    
    def SetExpiration(self, var):
        pass
    
    def Feed(self):
        pass
    
    def Kill(self):
        pass

def GetWatchdog():
    return dummyDog()

class Solenoid:
    def __init__(self, slot, port):
        self.slot = slot
        self.port = port
        print "Solenoid set up on slot " + str(slot) + " port " + str(port)
    
    def Get(self):
        return self.state
    
    def Set(self, state):
        self.state = state
        
        if state:
            print "port " + str(self.port) + " active"
        else:
            print "port " + str(self.port) + " inactive"

class Relay:
    def __init__(self, port):
        self.port = port
        print "Relay set up on port " + str(port)
        self.kForward = 1
        self.kOff     = 2
        self.kReverse = 3
    
    def Get(self):
        pass
    
    def Set(self, value):
        if value == self.kForward:
            print "relay forward"
        elif value == self.kOff:
            print "relay off"
        elif value == self.kReverse:
            print "relay backwards"

class Compressor:
    def __init__(self, port1, port2):
        pass
    def Start(self):
        print "compressor on"
    def Stop(self):
        print "compressor off"


