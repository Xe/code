/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.inputs;

import edu.wpi.first.wpilibj.Joystick;

/**
 *
 * @author sam
 */
public class SideWinder {
    
    private Joystick controller;

    public SideWinder (int port) {
        this.controller = new Joystick(port);
    }

    public double getXAxis() {
        return this.controller.getX();
    }

    public double getYAxis() {
        return this.controller.getY();
    }

    public double getTwistAxis() {
        return this.controller.getRawAxis(3);
    }

    public double getThrottle() {
        return this.controller.getRawAxis(4);
    }

    public boolean getTrigger() {
        return this.controller.getRawButton(1);
    }

    public boolean getTheLeftThumbButton() {
        return this.controller.getRawButton(2);
    }

    public boolean getTheRightTopButton() {
        return this.controller.getRawButton(3);
    }

    public boolean getTheRightBottomButton() {
        return this.controller.getRawButton(4);
    }

    public boolean getA() {
        return this.controller.getRawButton(5);
    }

    public boolean getB() {
        return this.controller.getRawButton(6);
    }

    public boolean getC() {
        return this.controller.getRawButton(7);
    }

    public boolean getD() {
        return this.controller.getRawButton(8);
    }

    public boolean getArrow() {
        return this.controller.getRawButton(9);
    }
    
}
