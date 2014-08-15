#Arm control funcs

try:
    import wpilib
except ImportError:
    import testing as wpilib

from outputs import speed_con

class arm:
    def __init__(self, port1, port2):
        try:
            #          slot       port
            self.arm = [port1[0], port1[1]]
            self.tower = [port2[0], port2[1]]
            
            self.arm = speed_con(self.arm[1], False, self.arm[0])
            self.tower = speed_con(self.tower[1], False, self.tower[0])
        except TypeError:
            self.arm = speed_con(port1, False)
            self.tower = speed_con(port2, False)
    
    def lower():
        self.arm.set_speed(-0.5)
        self.tower.set_speed(-0.5)
    
    def raise_():
        self.arm.set_speed(0.5)
        self.tower.set_speed(0.5)
    
    def pick_up():
        self.arm.set_speed(0.25)
    
    def put_down():
        self.arm.set_speed(-0.25)

    def stop():
        for n in [self.arm, self.tower]:
            n.set_speed(0)
    
    def place_dingus(robot):
        robot.stop()
        self.raise_()
        wpilib.Wait(0.25)
        self.stop()
        robot.tankDrive(0.25, 0.25)
        wpilib.Wait(0.125)
        robot.stop()
        self.put_down()
        self.lower()
        wpilib.Wait(0.125)
        robot.tankdrive(-0.5, -0.5)
        wpilib.Wait(0.125)
        self.pick_up()
        wpi.Wait(0.25) #TODO: Find out how long it takes for robot to turn self around
        self.stop()
        robot.tankDrive(0.5, -0.5)
        wpi.Wait(0.3)
