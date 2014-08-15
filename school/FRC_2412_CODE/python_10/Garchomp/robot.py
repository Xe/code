#!/ni-rt/system/FRC_UserProgram.out
#I know the preceeding line is not nessecary, but it makes me feel better because this is the main code file

try:
    import wpilib as wpi
    import vision
except:
    import testing as wpi

#shiny = vision.PCVideoServer()

import outputs as outs
import inputs  as ins
import util
import autonomous_driver as ad
util.sep()

stick1 = ins.Xbox(1)
#stick2 = ins.Attack3(2)
util.sep()

gun = outs.relay(2)
outs.initComp()
util.sep()

#drive = outs.Driver(1,2,3,4)
drive = outs.Driver(1,2)
lsg = ins.line_sensor_group(6,7,8)
util.sep()

#def readForDog():
#    return util.readFileandSplit("config.ini", 0, "True")

#doggy = readForDog()
doggy = True
util.sep()

def init():
    print("Cool stuff done")

def checkRestart():
    if stick1.joy.GetRawButton(8):  
        #if crazy stuff happens, hit button 10 (start on xbox360?)
        raise RuntimeError("Restart")

def disabled():
    print("Robot Disabled For Stuff")
    while wpi.IsDisabled():
        checkRestart()
        wpi.Wait(0.01)

def autonomous():
    print ("Robot autonomous")
    util.motd()
    wpi.GetWatchdog().SetEnabled(False)
    wpi.GetWatchdog().Kill()
    ad.go(drive, lsg)
    while wpi.IsAutonomous() and wpi.IsEnabled():
        checkRestart()
        wpi.Wait(0.01)
        drive.holonomicDriveRaw(1,0,0)

def teleop():
    print ("Robot Teleoperated")
    dog = wpi.GetWatchdog()
    global doggy
    
    if doggy:
        dog.SetEnabled(True)
        dog.SetExpiration(0.25)
    else:
        dog.SetEnabled(False)
        dog.Kill()

    while wpi.IsOperatorControl() and wpi.IsEnabled():
        dog.Feed()
        drive.arcadeDrive(stick1)     #a quiet jaunt around the park
        checkRestart()
        wpi.Wait(0.01)

class myRobot(wpi.SimpleRobot):
    def RobotInit(self):
        init()
        print("Inti\'d")

    def Autonomous(self):
        autonomous()

    def OperatorControl(self):
        teleop()

    def Disabled(self):
        disabled()

###########
#IMPORTANT#
###########
"""
  DONT TOUCH THE RUN OR RUN_OLD FUNCTION, STUFF WILL BREAK.
  YOU HAVE BEEN WARNED
"""

def run_old():
    """Main loop, old"""
    while 1:
        if wpi.IsDisabled():
            print("Running disabled()")
            disabled()
            while wpi.IsDisabled():
                wpi.Wait(0.01)
        elif wpi.IsAutonomous():
            print("Running autonomous()")
            autonomous()
            while wpi.IsAutonomous() and wpi.IsEnabled():
                wpi.Wait(0.01)
        else:
            print("Running teleop()")
            teleop()
            while wpi.IsOperatorControl() and wpi.IsEnabled():
                wpi.Wait(0.01)

def run():
    """current main loop"""
    robot = myRobot()
    robot.StartCompetition()
