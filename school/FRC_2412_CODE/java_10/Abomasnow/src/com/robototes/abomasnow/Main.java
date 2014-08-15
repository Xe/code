package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;
import edu.wpi.first.wpilibj.*;

public class Main extends SimpleRobot {
    Driver robot = new Driver(1,2);
    Xbox user = new Xbox(1);
    LineSensorGroup lsg = new LineSensorGroup(1,2,3);
    Switch dead = new Switch(4);

    Main() {
        Utils.initCam();
    }

    //Enumerates drive control state (Tank or Arcade)
    public void autonomous()
    {
        getWatchdog().feed();
        int last = 2;
        while(this.isEnabled() && this.isAutonomous()) {
            getWatchdog().feed();
            if(lsg.getMiddle()) {
                last = 2;
            } if (lsg.getLeft()) {
                last = 1;
            } if (lsg.getRight()) {
                last = 3;
            }

            if (lsg.allAreOn()) {
                if (last == 1) {
                    robot.hackedDrive(-0.5, 0.1);
                    System.out.println("Left");
                } else if (last == 2) {
                    robot.hackedDrive(-0.7, 0);
                    System.out.println("Forward");
                } else if (last == 3) {
                    robot.hackedDrive(-0.5, -0.1);
                    System.out.println("Right");
                }
            }

            if (lsg.getLeft() == true && lsg.getMiddle() == true && lsg.getRight() == true) {
                robot.stop();
                System.out.println("WE STOPPING");
                break;
            }

            System.out.println(last);
        }
        System.out.println("Exited");
    }

    public void operatorControl() {
        getWatchdog().feed();
        while(this.isEnabled() && this.isOperatorControl()) {
            robot.tankDrive(user);
            getWatchdog().feed();
        }
    }
}
