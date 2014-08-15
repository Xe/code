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
public class ConstantMotor {
    private Victor magnet;

    /*
     * Constructor that just makes the ConstantMotor go forward
     */
    public ConstantMotor(int slot) {
        magnet = new Victor(slot);
        this.start();
        System.out.println("ConstantMotor enabled on slot " + slot);
    }

    /*
     * Call this with false to make the ball magnet go backwards
     */
    public ConstantMotor(int slot, boolean forwards) {
        magnet = new Victor(slot);

        System.out.println("ConstantMotor enabled on slot " + slot);

        if(forwards) {
            this.start();
        } else {
            this.reverse();
        }
    }

    /*
     * makes it go forwards
     */
    public void start() {
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
}
