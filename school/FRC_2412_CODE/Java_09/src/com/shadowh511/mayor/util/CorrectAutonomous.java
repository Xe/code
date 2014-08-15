package com.shadowh511.mayor.util;

import com.shadowh511.mayor.outputs.*;
import com.shadowh511.mayor.inputs.*;

/**
 *
 * @author sam
 */
public class CorrectAutonomous{
    Driver wheels;
    Switch balls; 
    Firing gun;
    Logger log = new Logger("Auto");
    NosyParent arrgh = new NosyParent();
    ItGonnaDoIt thread = new ItGonnaDoIt();
    int ballNum = 0;
    
    public CorrectAutonomous(Driver x, Switch y, Firing z, int NumOfBalls){
        this.wheels = x;
        this.balls = y;
        this.gun = z;
        this.ballNum = NumOfBalls;
        //Thread.yield();
    }
    
    private class ItGonnaDoIt extends Thread {

        private boolean isAlive = true;

        public void run() {
            while(isAlive){
                wheels.drive(0.5);

                if (balls.ternaryGet()) {
                    wheels.stop();
                    gun.autoFire();
                    this.yield();
                }
            }
        }

        public void kill() {
            isAlive = false;
            Thread.yield();
        }
    }

    public void go() {
        thread.setPriority(Thread.MAX_PRIORITY);
        thread.run();
    }

    public void kill() {
        thread.interrupt();
        thread.kill();
    }
}
