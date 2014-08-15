/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.outputs;

import edu.wpi.first.wpilibj.Timer;

/**
 *
 * @author sam
 */
public class Firing {
    private MySolenoid kicker = new MySolenoid(2,3);
    private MySolenoid lock = new MySolenoid(1);
    static Firing instance = new Firing();

    public void tap() {
        kicker.extend();
    }

    public void unTap() {
        kicker.retract();
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

    public Firing getInstance() {
        return this.instance;
    }
}
