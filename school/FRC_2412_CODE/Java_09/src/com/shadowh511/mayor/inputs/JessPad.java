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
public class JessPad {
    private Joystick controller;

    public JessPad(int port) {
        System.out.println("JessPad started on port " + port);
        controller = new Joystick(port);
    }

    public double getLeftStickY() {
        return -this.controller.getRawAxis(2);
    }

    private double calcBuffer(double n) {
        double buffer = 0.2;
        if (Math.abs(n) < buffer) {
            n = 0.000000000000000;
        }
        return n;
    }

    public double getLeftStickX() {
        return this.controller.getRawAxis(1);
    }

    public double getRightStickY() {
        return this.controller.getRawAxis(4);
    }

    public double getRightStickX() {
        return this.controller.getRawAxis(3);
    }

    public boolean get1() {
        return this.controller.getRawButton(1);
    }

    public boolean get2() {
        return this.controller.getRawButton(2);
    }

    public boolean get3() {
        return this.controller.getRawButton(3);
    }

    public boolean get4() {
        return this.controller.getRawButton(4);
    }

    public boolean getRightFrontBumper() {
        return this.controller.getRawButton(6);
    }

    public boolean getRightBackBumper() {
        return this.controller.getRawButton(8);
    }

    public boolean getLeftFrontBumper() {
        return this.controller.getRawButton(5);
    }

    public boolean getLeftBackBumper() {
        return this.controller.getRawButton(7);
    }

    public boolean getSelect() {
        return this.controller.getRawButton(9);
    }

    public boolean getStart() {
        return this.controller.getRawButton(10);
    }

    public boolean getLeftStickButton() {
        return this.controller.getRawButton(11);
    }

    public boolean getRightStickButton() {
        return this.controller.getRawButton(12);
    }

    public boolean padXPositive() {
        if (this.controller.getRawAxis(5) > 0.1) {
            return true;
        } else {
            return false;
        }
    }

    public boolean padXNegative() {
        if (this.controller.getRawAxis(5) < -0.1) {
            return true;
        } else {
            return false;
        }
    }

    public boolean padYPositive() {
        if (this.controller.getRawAxis(6) > 0.1) {
            return true;
        } else {
            return false;
        }
    }

    public boolean padYNegative() {
        if (this.controller.getRawAxis(6) < -0.1) {
            return true;
        } else {
            return false;
        }
    }
}
