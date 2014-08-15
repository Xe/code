package com.shadowh511.mayor.outputs;

import edu.wpi.first.wpilibj.Timer;
import com.shadowh511.mayor.util.EasyDriverStationLCD;

public class FiringThread extends Thread {
    private MySolenoid kicker = new MySolenoid(2,3);
    private MySolenoid lock = new MySolenoid(1);
    private EasyDriverStationLCD lcd = new EasyDriverStationLCD();

    public boolean run = false;
    public boolean ready = true;
    public boolean lockmech = false;
    public boolean kickerOut = false;

    public FiringThread() {
        System.out.println("FiringThread Started");
    }

    public void lockGun() {
        lock.extend();
        lockmech = true;
        Thread.yield();
    }

    public void unLock() {
        lock.retract();
        lockmech = false;
        Thread.yield();
    }

    public void punch() {
        kicker.extend();
        kickerOut = false;
        Thread.yield();
    }

    public void unPunch() {
        kicker.retract();
        kickerOut = true;
        Thread.yield();
    }

    public void timerFire(double delay) {
        lock.extend();
        Timer.delay(0.25);
        kicker.extend();
        Timer.delay(delay);
        lock.retract();
        kicker.retract();
        Thread.yield();
    }

    public void autoFire() {
        ready = false;
        lockGun();
        Timer.delay(0.25);
        punch();
        Timer.delay(0.5);
        unLock();
        Timer.delay(0.5);
        unPunch();
        Timer.delay(2);
        ready = true;
        Thread.yield();
    }

    public void autoTap() {
        ready = false;
        this.punch();
        Timer.delay(0.75);
        this.unPunch();
        Timer.delay(2);
        ready = true;
        Thread.yield();
    }

    public boolean getState(int number){
        boolean state = true;

        if(number == 1) {
            state = lock.get();
        } if(number == 2) {
            state = kicker.get();
        }
        Thread.yield();
        return state;
    }

    public void kill() {
        this.run = false;
        Thread.yield();
    }

    public void run() {
        this.run = true;
        while(run) {
            Timer.delay(0.01);
        }
    }
}
