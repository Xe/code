#not the main class

try:
    import wpilib as wpi
except:
    import testing as wpi

import util

def Driver(p1, p2, p3 = None, p4 = None):
    print("Driver switcher function called")
    if p3 == p4 == None:
        print("Detected Two-Motor setup\n")
        return Driver_2(p1, p2)
    else:
        print("Detected Four-Motor setup\n")
        return Driver_4(p1, p2, p3, p4)

class Driver_2:
    """Driver class for two motor robots"""
    def __init__(self, port1, port2):
        self.L = speed_con(port1, True)
        self.R = speed_con(port2, True)
        self.vicList = [self.L, self.R]
        print("Two-wheeled driver on ports " + str(port1) + " and " + str(port2))
    
    def stop(self):
        print("stop called")
        for motor in self.vicList:
            motor.set_speed(0)
    
    def tankDriveRaw(self, left, right=0):
        self.L.set_speed(left)
        self.R.set_speed(-right)
        print ("tankDriveRaw at " + str(left) + " and " + str(right))
    
    def arcadeDriveRaw(self, turn, power):
        left = power + turn
        right = power - turn
        print("arcadeDriveRaw at " + str(power) + " and " + str(turn))
        self.tankDriveRaw(left, right)
    
    def tankDrive(self, ls, rs):
        return self.tankDriveRaw(ls.getStickAxes()[1], rs.getStickAxes()[1])
        
    def arcadeDrive(self, stick):
        return self.arcadeDriveRaw(stick.getStickAxes()[0], stick.getStickAxes()[1])

class Driver_4:
    """Driver class that manages the Motor controllers"""
    def __init__(self, port1, port2, port3, port4):
        """Depends on four motors, will make two-motor version asap"""
        self.FL = speed_con(port1, True)
        self.FR = speed_con(port2, True)
        self.RL = speed_con(port3, True)
        self.RR = speed_con(port4, True)
        self.vicList = [self.FL, self.FR, self.RL, self.RR]
    
    def stop(self):
        """Stops the robot, useful for everything"""
        self.tankDriveRaw(0, 0)
        print("Robot Stopped")
    
    def tankDrive(self, stick, stick2=None):
        """Takes two stick list inputs [x,y]"""
        left =  0
        right = 0
        
        if stick2 == None:
            try:
                left = -stick.getStickAxes()[1]
                right = -stick.getStickAxes()[4]
            except:
                left = -stick.getStickAxes()[1]
                right = -stick.getStickAxes()[3]
        else:
            left = stick.getStickAxes()[1]
            right = stick2.getStickAxes()[1]
        
        self.vicList[0].set_speed(left)
        self.vicList[1].set_speed(-right)
        self.vicList[2].set_speed(left)
        self.vicList[3].set_speed(-right)
        
        print("tankDrive at " + str(left) + " and " + str(right))
            
    def holonomicDrive(self, xboxController):
        """a nice wrapper"""
        power = xboxController.getAllAxes[1]  #y-axis, first stick
        strafe = xboxController.getAllAxes[0] #x-axis, first stick
        spin = xboxController.getAllAxes[3]   #x-axis, second stick (x...[2] is the trigger axis)
      
        fl = power + strafe + spin
        fr = power - strafe - spin
        rl = power - strafe + spin
        rr = power + strafe - spin
        
        print("holonomicDrive called")
        
        self.FL.set_speed(fl)
        self.FR.set_speed(fr)
        self.RL.set_speed(rl)
        self.RR.set_speed(rr)  
    
    def tankDriveRaw(self, left, right):
        self.vicList[0].set_speed(left)
        self.vicList[1].set_speed(-right)
        self.vicList[2].set_speed(left)
        self.vicList[3].set_speed(-right)
        print("tankDriveRaw at " + str(left) + " and " + str(right))

    def holonomicDriveRaw(self, power, strafe=0, spin=0):
        fl = power + strafe + spin
        fr = power - strafe - spin
        rl = power - strafe + spin
        rr = power + strafe - spin
        
        max = util.max_of_4(fl, fr, rl, rr)
        
        fl = fl/max
        fr = fr/max
        rl = rl/max
        rr = rr/max
        
        print('holonomicDriveRaw called')
        
        self.FL.set_speed(fl)
        self.FR.set_speed(fr)
        self.RL.set_speed(rl)
        self.RR.set_speed(rr)
        
class dual_solenoid:
    def __init__(self, port1, port2):
        self.one = single_solenoid(port1)
        self.two = single_solenoid(port2)
    
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
        
class single_solenoid:
    def __init__(self, port):
        self.port = port
        print("single_solenoid set up on port " + str(port))
        self.noid = wpi.Solenoid(8, port)
        comp_needed = True
    
    def extend(self):
        self.noid.Set(True)
        print ("Solenoid on port " + str(self.port) + " active")
    
    def retract(self):
        self.noid.Set(False)
        print ("Solenoid on port " + str(self.port) + " not active")
        
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

class EL_wire(object):
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

class speed_con:
    def __init__(self, port, jaguar, slot=4):
        print("speed_con obj on port " + str(port) + " active")
        
        if jaguar == True:
            self.vic = wpi.Jaguar(port)
            print("is jaguar\n")
        else:
            self.vic = wpi.Victor(port)
            print("is victor\n")

    def set_speed(self, value):
        self.vic.Set(self.limit(value))
    
    def limit(self, n):
        if n > 1.0:
            return 1.0
        elif n < -1.0:
            return -1.0
        else:
            return n
