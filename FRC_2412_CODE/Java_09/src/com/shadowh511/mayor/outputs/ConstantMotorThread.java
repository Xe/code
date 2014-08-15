/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.outputs;

import edu.wpi.first.wpilibj.Victor;

/**
 *
 * @author sam
 */
public class ConstantMotorThread extends Thread {
    private Victor magnet;
    private boolean run = true;
    public boolean forward = true;
    public boolean stopping = false;

    /*
     * Constructor that just makes the ConstantMotor go forward
     */
    public ConstantMotorThread(int slot) {
        magnet = new Victor(slot);
        forward = true;
        this.go();
    }

    /*
     * Call this with false to make the ball magnet go backwards
     */
    public ConstantMotorThread(int slot, boolean forwards) {
        magnet = new Victor(slot);
        forward = forwards;
        this.go();
    }

    /*
     * makes it go forwards
     */
    public void go() {
        this.magnet.set(0.2);
    }

    /*
     * Stops the ConstantMotor
     */
    public void stop() {
        this.magnet.set(0);
    }

    /*
     * makes it go backwards
     */
    public void reverse() {
        this.magnet.set(-0.2);
    }

    public void halt() {
        this.stopping = true;
    }

    public void run() {
        while (run) {
            if(this.forward) {
                this.go();
            } else if (this.stopping) {
                this.stop();
            }
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
            }
        }
    }
}
