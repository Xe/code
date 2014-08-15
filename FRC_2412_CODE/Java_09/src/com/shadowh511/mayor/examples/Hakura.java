package com.shadowh511.mayor.examples;

import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.inputs.Attack3;
import edu.wpi.first.wpilibj.Relay;
import edu.wpi.first.wpilibj.Compressor;
import edu.wpi.first.wpilibj.Jaguar;
import edu.wpi.first.wpilibj.SimpleRobot;

public class Hakura extends SimpleRobot { 
    /*
     * This is equivalent code for last year's robot (2009)
     *
     * named after May's Japanese name (Emerald Player Character trainer)
     */

    Attack3 left = new Attack3(1);
    Attack3 right = new Attack3(2);
    Driver robot = new Driver(3,4);
    Relay gun = new Relay(2);
    Jaguar tensioner = new Jaguar(5);
    Compressor squish = new Compressor(1,1);
    Compressor renee = new Compressor(1,8);

    public void operatorControl() {
        squish.start();
        getWatchdog().setEnabled(false);
        getWatchdog().kill();

        while (this.isOperatorControl() && this.isEnabled()) {
            robot.hackedDrive(left.getX(), -left.getY());
            tensioner.set(right.getX());

            if (right.getTrigger() && left.getTrigger()) {
                gun.set(Relay.Value.kForward);
                System.out.println("F13R!!!");
                edu.wpi.first.wpilibj.Timer.delay(0.125);
                gun.set(Relay.Value.kOff);
            }
        }
        
        squish.stop();
    }
}