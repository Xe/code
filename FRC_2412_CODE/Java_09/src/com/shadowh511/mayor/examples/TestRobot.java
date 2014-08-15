package com.shadowh511.mayor.examples;
//Super-basic, super-swanky, super-awesome

import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.inputs.Xbox;
import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;

public class TestRobot extends SimpleRobot {
    Driver robot = new Driver(3,4); //Left motor on port 3, right motor on port 4
    Xbox user = new Xbox(1); //Joe Schmoe plugged an xbox controller into port 1

    public void autonomous() {
        robot.drive(0.5);  //Drives in a square
        Timer.delay(0.5);
        robot.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        robot.drive(0.5);
        Timer.delay(0.5);
        robot.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        robot.drive(0.5);
        Timer.delay(0.5);
        robot.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        robot.drive(0.5);
        Timer.delay(0.5);
        robot.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
    }

    public void operatorControl() {
        while(this.isEnabled()) {
            robot.hackedDrive(user.getLeftStickY(), user.getLeftStickX());
        }
    }
}
