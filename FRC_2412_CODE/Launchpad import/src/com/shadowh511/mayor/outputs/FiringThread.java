package com.shadowh511.mayor.outputs;

import edu.wpi.first.wpilibj.Timer;
import edu.wpi.first.wpilibj.Compressor;

public class FiringThread extends Thread {
    private MySolenoid kicker = new MySolenoid(2,3);
    private MySolenoid lock = new MySolenoid(1);
    private Compressor compressor = new Compressor(1,1);
    private boolean run = true;

    public FiringThread() {
        compressor.start();
        System.out.println("FiringThread Started");
    }

    public void lockGun() {
        lock.extend();
    }

    public void unLock() {
        lock.retract();
    }

    public void punch() {
        kicker.extend();
    }

    public void unPunch() {
        kicker.retract();
    }

    public void timerFire(double delay) {
        lock.extend();
        Timer.delay(0.25);
        kicker.extend();
        Timer.delay(delay);
        lock.retract();
        kicker.retract();
    }

    public void autoFire() {
        lockGun();
        Timer.delay(0.25);
        punch();
        Timer.delay(0.125);
        unLock();
        Timer.delay(0.75);
        unPunch();
        Timer.delay(2);
    }

    public void autoTap() {
        this.punch();
        Timer.delay(0.75);
        this.unPunch();
        Timer.delay(2);
    }

    public boolean getState(int number){
        boolean state = true;

        if(number == 1) {
            state = lock.get();
        } if(number == 2) {
            state = kicker.get();
        }
        return state;
    }

    public void kill() {
        this.run = false;
    }

    public void run() {
        while(run) {
            Timer.delay(0.0000001);
        }
    }
}
