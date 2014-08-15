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
public class Attack3 {
    private Joystick controller;

    public Attack3(int port) {
        controller = new Joystick (port);
    }

    public double getX() {
        return this.controller.getRawAxis(2);
    }

    public double getY() {
        return this.controller.getRawAxis(1);
    }

    public double getThrottle() {
        return this.controller.getRawAxis(3);
    }

    public boolean getTrigger() {
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

    public boolean get5() {
        return this.controller.getRawButton(5);
    }

    public boolean get6() {
        return this.controller.getRawButton(6);
    }

    public boolean get7() {
        return this.controller.getRawButton(7);
    }

    public boolean get8() {
        return this.controller.getRawButton(8);
    }

    public boolean get9() {
        return this.controller.getRawButton(9);
    }

    public boolean get10() {
        return this.controller.getRawButton(10);
    }

    public boolean get11() {
        return this.controller.getRawButton(11);
    }
}
