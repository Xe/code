/*
 * This code is what the mayor will run at competition
 * :D
 */

package com.shadowh511.mayor;

import com.shadowh511.mayor.outputs.*;
import com.shadowh511.mayor.inputs.*;
import com.shadowh511.mayor.utils.*;
import edu.wpi.first.wpilibj.Compressor;
import edu.wpi.first.wpilibj.Watchdog;
import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;

 /**
 * The VM is configured to automatically run this class, and to call the
 * functions corresponding to each mode, as described in the SimpleRobot
 * documentation. If you change the name of this class or the package after
 * creating this project, you must also update the manifest file in the resource
 * directory.
 */
public class mekanum extends SimpleRobot {
    Xbox sideWing = new Xbox(1);  //xbox 360 controller
    GreenAsia greenAsia = new GreenAsia(2); //ps2 fighter joypad
    Compressor compressor = new Compressor(1,1);
    Watchdog Q = Watchdog.getInstance();
    Driver robot = new Driver(1,2,3,4,true);
    FiringThread gun = new FiringThread();
    EasyDriverStation ds = new EasyDriverStation();
    EasyDriverStationLCD lcd = new EasyDriverStationLCD();

    boolean ready = false;
    boolean fired = false;
    long loopNum = 0;
    long time = 0;
    long hyperTime = 0;
    
    /**
     * This function is called once each time the robot enters autonomous mode.
     */
    public void autonomous() {
        Q.kill();
        Q.setEnabled(false);
        compressor.start();
        gun.lockGun();
        Timer.delay(0.5);
        gun.punch();
        ready = true;
        int i = 0;

        while(isEnabled()) {
            lcd.writeOnLine(1, ""+ i , 0);
            lcd.draw();
            i++;
        }
    }

    /**
     * This function is called once each time the robot enters operator control.
     */
    public void operatorControl() {
        compressor.start();
        Q.setEnabled(true);
        Q.feed();
        System.out.println("ARE YOU READY TO RUMMMMBLE???");
        int i = 0;
        gun.unLock();
        gun.unPunch();

        while(isEnabled()) {
            Q.feed();
            robot.mecanumDrive(sideWing);
            lcd.writeOnLine(2, ""+i, 0);
            
            if (fired) {
                gun.unLock();
                if(Timer.getUsClock() - time > 750000) {
                    gun.unPunch();
                    Q.feed();
                    fired = false;
                    System.out.println("Gun resetting");
                }
            }
            lcd.draw();

            if (greenAsia.getR2()) {
                gun.unLock();
                gun.unPunch();
            }

            if (greenAsia.getCircle()) {
                if (ready && (Timer.getUsClock() - time > 2000000) && (Timer.getUsClock() - hyperTime > 250000)) {
                    gun.unLock();
                    time = Timer.getUsClock();
                    System.out.println("Gun unlocking");
                    ready = false;
                    fired = true;
                    Q.feed();
                } else if (!fired) {
                    gun.lockGun();
                    hyperTime = Timer.getUsClock();
                    System.out.println("Gun locking");
                    gun.punch();
                    ready = true;
                    Q.feed();
                }
            } else if (greenAsia.getCross()) {
                if ((Timer.getUsClock() - time > 2000000) && !fired) {
                    gun.punch();
                    time = Timer.getUsClock();
                    fired = true;
                    System.out.println("Gun tapping");
                    Q.feed();
                }
            }            
            Q.feed();
            i++;
        }
    }
}
