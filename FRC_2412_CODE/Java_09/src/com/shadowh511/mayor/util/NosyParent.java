package com.shadowh511.mayor.util;

/**
 *
 * @author sam
 */

import edu.wpi.first.wpilibj.Timer;

public class NosyParent extends Thread {
    final double delay = 0.5;
    long checkinTime = 0;
    boolean enabled = false;

    public void checkIn() {
        this.checkinTime = Timer.getUsClock();
        Thread.yield();
    }

    public void setup() {
        this.enabled = true;
        Thread.yield();
    }

    public void run() {
        if (this.enabled){
            if (Timer.getUsClock()-checkinTime > delay) {
                System.out.println("Something might be hanging the System");
            }
        }
    }
}
