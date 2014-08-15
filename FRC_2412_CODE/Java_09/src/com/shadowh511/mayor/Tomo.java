package com.shadowh511.mayor;

/*
 * For use with a barebones robot.  It has two motors and two jags 1 and 2
 * respectively.  It also has a switch to get input for a sensor so it can
 * automatically kick balls, if it had such a mechanism;
 */

import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;
import com.shadowh511.mayor.inputs.Attack3;
import com.shadowh511.mayor.inputs.Switch;
import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.outputs.ConstantMotorThread;
import com.shadowh511.mayor.outputs.FiringThread;
import com.shadowh511.mayor.util.EasyDriverStationLCD;

public class Tomo extends SimpleRobot {
    Attack3 driver_L = new Attack3(1);
    Attack3 driver_R = new Attack3(2);
    Driver motors = new Driver(1,2);
    EasyDriverStationLCD lcd = new EasyDriverStationLCD();
    Switch balls = new Switch(4);
    ConstantMotorThread magnet = new ConstantMotorThread(7);
    FiringThread gun = new FiringThread();
    
    long loops;

    public void robotInit() {
        magnet.start();
        System.out.println("ConstantMotor thread started");
        getWatchdog().setEnabled(false);
        getWatchdog().kill();
    }

    /**
     * This function is called once each time the robot enters autonomous mode.
     */
    public void autonomous() {
        this.robotInit();
        getWatchdog().setEnabled(false);
        getWatchdog().kill();

        System.out.println("Autonomous Enabled");

        lcd.clear();
        lcd.writeOnLine(0, "AUTONOMOUS", 0);
        lcd.draw();

        motors.drive(0.5);  //Drives in a square, hopefully
        Timer.delay(0.5);
        motors.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        motors.drive(0.5);
        Timer.delay(0.5);
        motors.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        motors.drive(0.5);
        Timer.delay(0.5);
        motors.tankDrive(0.5, -0.5);
        Timer.delay(0.25);
        motors.drive(0.5);
        Timer.delay(0.5);
        motors.tankDrive(0.5, -0.5);
        Timer.delay(0.25);

        System.out.println("Automous Disabled");
    }

    /**
     * This function is called once each time the robot enters operator control.
     */
    public void operatorControl() {
        this.robotInit();
        getWatchdog().setEnabled(false);
        getWatchdog().kill();

        lcd.clear();
        lcd.writeOnLine(0, "TELEOPERATED", 0);
        lcd.draw();

        loops = 0;

        System.out.println("Teleop Enabled");
        
        while(isEnabled()) {
            lcd.writeOnLine(1, ""+loops, 0);

            System.out.println("" + balls.ternaryGet());

//            motors.tankDrive(driver_L, driver_R);
            loops ++;

            if(balls.ternaryGet()) {
                lcd.writeOnLine(2, "TRUE ", 0);
            } else {
                lcd.writeOnLine(2, "FALSE", 0);
            }
            
            lcd.draw();
        }

        System.out.println("Teleop Disabled");
    }
}
