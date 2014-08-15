#inputs.py

def system():
    self.runPlace = None
    
try:
    import wpilib as wpi
    system.runPlace = 0 # robot
    print("inputs running on Robot")
except:
    import testing as wpi
    system.runPlace = 1 # laptop
    print("inputs running on Laptop")

class Attack3:
    def __init__(self, port):
        self.port = port
        self.joy  = wpi.Joystick(self.port)
        
        print "New Attack3 configured at port " + str(port)
    
    def getAxies(self, x_axis, y_axis):
        """Returns a list in the format of [arg1, arg2]"""
        resultant = list()
        resultant.append(self.joy.GetRawAxis(x_axis))
        resultant.append(self.joy.GetRawAxis(y_axis))
        
        return resultant
    	
    def getStickAxes(self):
        return self.getAxies(2,1)
    
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
      
      print "New Attack3 configured at port " + str(port)
    
    def bufferIt(n):
        if abs(n) < 0.2:
            n = 0
        return n
      
    def getAxies(self, x_axis, y_axis):
        """Returns a list in the format of [arg1, arg2]"""
        resultant = list()
        resultant.append(bufferIt(self.joy.GetRawAxis(x_axis)))
        resultant.append(bufferIt(self.joy.GetRawAxis(y_axis)))
        return resultant
    
    def getAllAxes(self):
        r = list()
        for n in range(1,6):
            r.append(bufferIt(self.joy.GetRawAxis(n)))
	    return r

    def getButton(self, button):
        return self.joy.GetRawButton(button)
    
    def getButtonList(self):
        r = list()
        
        for n in range(1,9):
            r.append(self.joy.getRawButton(n))
	
        return r
