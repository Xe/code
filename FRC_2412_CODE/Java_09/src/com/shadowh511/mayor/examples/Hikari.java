package com.shadowh511.mayor.examples;

import edu.wpi.first.wpilibj.SimpleRobot;
import com.shadowh511.mayor.inputs.Xbox;
//import com.shadowh511.mayor.inputs.Switch;
import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.outputs.FiringThread;
//import com.shadowh511.mayor.util.Camera;
//import com.shadowh511.mayor.util.Constants;
import edu.wpi.first.wpilibj.Compressor;

public class Hikari extends SimpleRobot {
    /*
     * Mecanum wheels
     * 
     * This is equivalent code to the code used at the robotics competition (2010)
     * well, it has a LOT of improvments though
     *
     * Named after the Japanese name for the female trainer in the D/P/Pl era
     * and just to clarify, i mean the player character, literal translation is
     * dawn
     *
     * What the Robot Needs for this to work:
     *  - Camera
     *  - a Switch right over the ball firing agent
     *  - An Xbox 360 controller plugged into joystick port 1
     *  - The standard compliment of stuff on Francie (Mayor Mc. Cheese)
     */
    
    Driver motors = new Driver(1,2,3,4,true);
    FiringThread gun = new FiringThread();
    Xbox driver = new Xbox(1);
    //Switch ball = new Switch(4);
    //EasyDriverStationLCD lcd = new EasyDriverStationLCD();
    Compressor compressor = new Compressor(4,1,4,1);
    //Camera cam = new Camera();

    public void initRobot() {
        compressor.start();
        getWatchdog().setEnabled(false);
        getWatchdog().kill();
        if (!gun.run) {  //just make sure there is only one firing thread
            gun.run = true;
            gun.start();
        }
        //lcd.clear();
    }

//    public void autonomous() { //For running and Gunning xD
//        initRobot();
//        int balls = 2; //Assuming middle spot on the field, we did this a lot anyway
//
//        while (balls > 0 && this.isEnabled()) { //just a simple loop
//            if (ball.get()) { //if the robot runs into a ball
//                motors.stop();  //dont move
//
//                int reccomendation = cam.howHardDoINeedToKick();
//                //I love this function name, i don't know why
//
//                if (reccomendation == Constants.Firing.soft) {
//                    gun.autoTap();
//                } else {
//                    gun.autoFire();
//                }
//
//                Timer.delay(3); //wait for the gun to do its thing
//                balls--;
//            } else {
//                motors.drive(Constants.Driving.halfImpulse); //just move otherwise
//            }
//        }
//    }

    public void operatorControl() {
        initRobot();
        
        while (this.isEnabled() && this.isOperatorControl()) { //i love loops <3
            motors.holonomicDrive(driver.getLeftStickY(),
                                -driver.getTriggerAxis()/2,
                                driver.getRightStickX()
                                ); //drive with mecanum skillz

            if (driver.getA()) { //if the gunner pushes the A button
                gun.autoTap();       //shoot the gun at low power
                System.out.println("tap");  //debug
                //lcd.writeOnLine(0, "Kick", 0);
            } else if (driver.getB()) { //if the gunner hits the B button
                gun.autoFire(); //shoot it hard
                System.out.println("shoot");
            }
            //lcd.writeOnLine(1, this.compressor.getPressureSwitchValue() ? "OFF" : "ON", 0);
            //lcd.draw();
            
        }
        motors.stop();
    }
}
