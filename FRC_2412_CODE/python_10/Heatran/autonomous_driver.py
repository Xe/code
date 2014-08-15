import inputs as ins
import util

def go(driver, lsg):
    
    while True:
        try:
            temp = lsg.get()
            if temp == ins.state.go_forward:
                print("Forward")
                driver.tankDrive(0.5, 0.5)
            elif temp == ins.state.go_left:
                print("Left")
                driver.tankDrive(-0.5, 0.5)
            elif temp == ins.state.go_right:
                print("Right")
                driver.tankDrive(0.5, -0.5)
            elif temp == ins.state.wtf:
                print("WTF?")
                driver.tankDrive(0,0)
                util.fail()
            elif temp == ins.state.stop:
                print("Stop")
                driver.tankDrive(0,0)
                break
            elif temp == ins.state.split_detected:
                driver.tankDrive(0,0)
                print("Split")
                
            
        except:
            util.fail()
            break
