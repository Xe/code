#inputs.py

from util import puts

class system:
    pass
    
def dead_buffer(n):
    if abs(n) < 0.1:
        return 0
    else:
        return n

try:
    import wpilib as wpi
    system.runPlace = 0 # robot
    puts("inputs running on Robot")

except ImportError:
    import testing as wpi
    system.runPlace = 1 # laptop
    puts("inputs running on Laptop")

class Attack3:
    self.is_xbox = False
    def __init__(self, port):
        self.port = port
        self.joy  = wpi.Joystick(self.port)
    
    def bufferIt(self, n):
        if abs(n) < 0.1:
            n = 0
        return n
    
    def getAxies(self, x_axis, y_axis):
        """Returns a list in the format of [arg1, arg2]"""
        resultant = list()
        resultant[0]=self.bufferIt(self.joy.GetRawAxis(x_axis))
        resultant[1]=self.bufferIt(self.joy.GetRawAxis(y_axis))
        
        return resultant
        
    def getStickAxes(self):
        return self.getAxies(1,2)
         
    def getThrottle(self):
        return self.joy.GetRawAxis(3)
    
    def getButtons(self):
        r = [] #r is of type list
        for n in range (1,12):
            r.append(self.joy.GetRawButton(n))
        
        #To the tune of ghostbusters:
        #When there's an annoying problem, that you need to solve? 
        #whatcha gonna use? 
        #A FOR LOOP!
        #when there's a lot of data
        #that you need to parse
        #whatcha gonna use?
        #A FOR LOOP!
        
        #I ain't afraid of no macs
        
        #XD
            
        return r
        
    def getTrigger(self):
        return self.getButtons()[0]
    
class Xbox():
    def __init__(self, port):
      self.port = port
      self.joy  = wpi.Joystick(self.port)
      self.a = 1
      self.b = 2
      self.x = 4
      self.y = 3
      self.lb = 6
      self.rb = 5
      self.back = 7
      self.start = 8
      self.is_xbox = False
      
      puts("New xbox360 Controller configured at port " + str(port) + "\n")
    
    def bufferIt(self, n):
        if abs(n) < 0.2:
            n = 0
        return n
      
    def getAxies(self, x_axis, y_axis):
        """Returns a list in the format of [arg1, arg2]"""
        resultant = list()
        resultant.append(self.bufferIt(self.joy.GetRawAxis(x_axis)))
        resultant.append(self.bufferIt(self.joy.GetRawAxis(y_axis)))
        return resultant
    
    def getStickAxes(self):
        r = list()
        for n in range(1,6):
            r.append(self.bufferIt(self.joy.GetRawAxis(n)))
        return r

    def get_button(self, button):
        return self.joy.GetRawButton(button)
    
    def get_button_list(self):
        r = list()
        
        for n in range(1,9):
            r.append(self.joy.GetRawButton(n))
    
        return r

class line_sensor:
    #no longer a stub
    def __init__(self, port, slot = 4):
        self.sensor = wpi.DigitalInput(slot, port)
    
    def get(self):
        return self.sensor.Get()

class state:
    """
    010 forward
    101 split detected
    110 go Left
    011 go right
    000 wtf?
    111 stop
    """
    go_forward =     (False, True, False)
    split_detected = (True, False, True)
    go_left =        (True, True, False)
    go_right =       (False, True, True)
    wtf =            (False, False, False)
    stop =           (True, True, True)

class line_sensor_group(object):
    def __init__(self, port1, port2, port3):
        self.lsGroup = (line_sensor(port1), line_sensor(port2), line_sensor(port3))
    def get():
        return [self.lsGroup[0].get(), self.lsGroup[1].get(), self.lsGroup[2].get()]
