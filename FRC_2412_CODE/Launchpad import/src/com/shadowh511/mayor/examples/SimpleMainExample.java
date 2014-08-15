package com.shadowh511.mayor.examples;

import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.inputs.Xbox;
import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;

public class SimpleMainExample extends SimpleRobot {
    Xbox sideWing = new Xbox(1);
    Driver robot = new Driver(1,2);

    public void autonomous() {
        getWatchdog().setEnabled(false);
        getWatchdog().kill();
        int balls = 2;
        while(balls > 0){
            robot.drive(0.75);
            Timer.delay(0.2);
            robot.drive(-.25);
            Timer.delay(0.25);
            balls --;
        }
        robot.stop();
    }

    public void operatorControl() {
        while (this.isOperatorControl() && this.isEnabled()) {
            robot.tankDrive(sideWing);
        }
    }
}