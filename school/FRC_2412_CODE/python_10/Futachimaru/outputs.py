#not the main class

try:
    import wpilib as wpi
except:
    import testing as wpi

class Driver:
    """Driver class that manages the Motor controllers"""
    def __init__(self, port1, port2, port3, port4):
        """Depends on four motors, will make two-motor version asap"""
        self.FL = wpi.Victor(port1)
        self.FR = wpi.Victor(port2)
        self.RL = wpi.Victor(port3)
        self.RR = wpi.Victor(port4)
        self.vicList = [self.FL, self.FR, self.RL, self.RR]
    
    def stop(self):
        """Stops the robot, useful for everything"""
        self.tankDriveRaw(0, 0)
        print("Robot Stopped")
    
    def tankDrive(self, stick, stick2):
        """Takes two stick list inputs [x,y]"""
        left = stick[1]
        right = stick2[1]
        
        self.vicList[0].Set(left)
        self.vicList[1].Set(-right)
        self.vicList[2].Set(left)
        self.vicList[3].Set(-right)
        
        print("tankDrive at " + str(left) + " and " + str(right))
            
    def holonomicDrive(self, xboxController):
        """a nice wrapper"""
        power = xboxController[1]  #y-axis, first stick
        strafe = xboxController[0] #x-axis, first stick
        spin = xboxController[3]   #x-axis, second stick (x...[2] is the trigger axis)
      
        fl = power + strafe + spin
        fr = power - strafe - spin
        rl = power - strafe + spin
        rr = power + strafe - spin
        
        print("holonomicDrive called")
        
        self.FL.Set(fl)
        self.FR.Set(fr)
        self.RL.Set(rl)
        self.RR.Set(rr)  
    
    def tankDriveRaw(self, left, right):
        self.vicList[0].Set(left)
        self.vicList[1].Set(-right)
        self.vicList[2].Set(left)
        self.vicList[3].Set(-right)
        print("tankDriveRaw at " + str(left) + " and " + str(right))

    def holonomicDriveRaw(self, power, strafe, spin):
        fl = power + strafe + spin
        fr = power - strafe - spin
        rl = power - strafe + spin
        rr = power + strafe - spin
        
        print('holonomicDriveRaw called')
        
        self.FL.Set(fl)
        self.FR.Set(fr)
        self.RL.Set(rl)
        self.RR.Set(rr)
        
class dualSolenoid:
    def __init__(self, port1, port2):
        self.one = singleSolenoid(port1)
        self.two = singleSolenoid(port2)
    
    def extend(self):
        self.one.extend()
        self.two.retract()
    
    def retract(self):
        self.one.retract()
        self.two.retract()
    
    def get(self):
        if self.one.get() and not self.two.get():
            return True
        else:
            return False
        
class singleSolenoid:
    def __init__(self, port):
        self.port = port
        print "singleSolenoid set up on port " + str(port)
        self.noid = wpi.Solenoid(8, port)
        comp_needed = True
    
    def extend(self):
        self.noid.Set(True)
    
    def retract(self):
        self.noid.Set(False)
        
    def get(self):
        return self.noid.Get()
        
class relay:
    def __init__(self, port):
        self.rel = wpi.Relay(port)
        self.port = port
        comp_needed = True
        print("ghetto relay class set up on port " + str(port))
    
    def forward(self):
        self.rel.Set(self.rel.kForward)
        print ("relay on port " + str(self.port) + " set to forward")
    
    def backward(self):
        self.rel.Set(self.rel.kReverse)
        print ("relay on port " + str(self.port) + " set to backward")
    
    def off(self):
        self.rel.Set(self.rel.kOff)
        print ("relay on port " + str(self.port) + " set to off")
        
comp_needed = False
comp = None

def initComp():
    comp = wpi.Compressor(1,1)
    comp.Start()

class ELWire(object):
    def __init__(self, port):
        self.port = port
        print ("EL Wire Driver set up on relay port " + str(port))
        self.relay = relay(port)
    
    def on():
        self.relay.forward()
        print ("EL Wire on, let the awesome commence")
    
    def off():
        self.relay.off()
        print ("EL Wire off, let the sadness be heard")
