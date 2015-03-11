/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.inputs;

import edu.wpi.first.wpilibj.Joystick;

/**
 *
 * @author Christine Dodrill
 */
public class Xbox {

    private Joystick controller;

    /*
     * Makes a new joystick object on port
     */
    public Xbox (int port) {
        controller = new Joystick(port);
    }

    /*
     * Returns the xbox 360 controller trigger axis
     */
    public double getTriggerAxis() {
        return this.controller.getRawAxis(3);
    }

    /*
     * Returns the xBox 360 controller buffered left stick X-axis
     */
    public double getLeftStickX() {
        return this.calcBuffer(this.controller.getRawAxis(1));
    }

    /*
     * The xbox 360 controller has a bad axis readout.  This function takes n
     * (this was when I didn't make long variable names).  It makes a value of
     * up to 0.2 (IE: barely touching it/ leaving it neutral) equal to zero.
     */
    private double calcBuffer(double n) {
        double buffer = 0.2;
        if (Math.abs(n) < buffer) {
            n = 0.000000000000000;
        }
        return n;
    }

    /*
     * Returns the buffered left stick y-axis
     */
    public double getLeftStickY() {
        return this.calcBuffer(this.controller.getRawAxis(2));
    }

    /*
     * Returns the buffered right stick X axis
     */
    public double getRightStickX() {
        return this.calcBuffer(this.controller.getRawAxis(4));
    }

    /*
     * Returns the buffered right stick Y axis
     */
    public double getRightStickY() {
        return this.calcBuffer(this.controller.getRawAxis(5));
    }

    /*
     * Returns the state of the A button (button 1)
     */
    public boolean getA() {
        return this.controller.getRawButton(1);
    }

    /*
     * Returns the state of the b button
     */
    public boolean getB() {
        return this.controller.getRawButton(2);
    }

    /*
     * Returns the state of the y button
     */
    public boolean getY() {
        return this.controller.getRawButton(3);
    }

    /*
     * returns the state of the x button
     */
    public boolean getX() {
        return this.controller.getRawButton(4);
    }

    /*
     * Returns the state of the right bumper
     */
    public boolean getRightBumper() {
        return this.controller.getRawButton(5);
    }

    /*
     * Returns the state of the left bumper
     */
    public boolean getLeftBumper() {
        return this.controller.getRawButton(6);
    }

    /*
     * Returns the state of the Back button
     */
    public boolean getBack() {
        return this.controller.getRawButton(7);
    }

    /*
     * Returns the state of the Start button
     */
    public boolean getStart() {
        return this.controller.getRawButton(8);
    }
}
