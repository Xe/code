#!/ni-rt/system/FRC_UserProgram.out
#I know the preceeding line is not nessecary, but it makes me feel better b/c this is the main code file

try:
    import wpilib as wpi
except:
    import testing as wpi

import outputs
import inputs

stick1 = inputs.Attack3(1)
stick2 = inputs.Attack3(2)

drive = outputs.Driver(1,2,3,4)

def checkRestart():
    if stick1.joy.GetRawButton(10):  #if crazy stuff happens, hit button 10
        raise RuntimeError("Restart")

def disabled():
    while wpi.IsDisabled():
        checkRestart()
        wpi.Wait(0.01)

def autonomous():
    wpi.GetWatchdog().SetEnabled(False)
    while wpi.IsAutonomous() and wpi.IsEnabled():
        checkRestart()
        wpi.Wait(0.01)

def teleop():
    dog = wpi.GetWatchdog()
    dog.SetEnabled(True)
    dog.SetExpiration(0.25)
    dog.SetEnabled(False)
    dog.Kill()

    while wpi.IsOperatorControl() and wpi.IsEnabled():
        dog.Feed()
        drive.tankDrive(stick1, stick2)     #a quiet jaunt around the park
        checkRestart()
        wpi.Wait(0.01)

###########
#IMPORTANT#
###########
"""
  DONT TOUCH THE RUN FUNCTION, STUFF WILL BREAK.
  YOU HAVE BEEN WARNED
"""

def run():
    """Main loop"""
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

