package com.shadowh511.mayor.examples;

import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Timer;
import com.shadowh511.mayor.inputs.Xbox;
import com.shadowh511.mayor.inputs.GreenAsia;
import com.shadowh511.mayor.inputs.Switch;
import com.shadowh511.mayor.outputs.Driver;
import com.shadowh511.mayor.outputs.FiringThread;

public class SimpleMecanumMainExample extends SimpleRobot {
    Driver motors = new Driver(1,2,3,4,true);
    FiringThread gun = new FiringThread();
    Xbox driver = new Xbox(1);
    GreenAsia gunner = new GreenAsia(2);
    Switch ball = new Switch(4);

    public void initRobot() {
        getWatchdog().setEnabled(false);
        getWatchdog().kill();
    }

    public void autonomous() {
        initRobot();
        int balls = 2; //Assuming middle spot on the feild, we did this a lot anyway

        while (balls > 0 && this.isEnabled()) { //just a simple loop
            if(ball.ternaryGet()) {
                motors.stop();
                gun.autoFire();
                Timer.delay(3);
                balls--;
            } else {
                motors.drive(0.25);
            }
        }
    }

    public void operatorControl() {
        initRobot();
        while(this.isEnabled() && this.isOperatorControl()) {
            motors.mecanumDrive(driver);
            if(gunner.getCross()) {
                gun.autoTap();
            } else if (gunner.getSquare()) {
                gun.autoFire();
            }
        }
    }
}