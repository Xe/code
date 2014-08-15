/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.Joystick;

/**
 *
 * @author sam
 */
public class Attack3 {
    Joystick joy;

    public Attack3(int port) {
        joy = new Joystick(port);
    }

    double[] getAxes(int x_axis, int y_axis) {
        double[] res = new double[2];
        res[0] = joy.getRawAxis(x_axis);
        res[1] = joy.getRawAxis(y_axis);

        return res;
    }

    double[] getStickAxes() {
        return getAxes(1,2);
    }

    boolean getTrigger() {
        return get(1);
    }

    boolean get(int n) {
        return joy.getRawButton(n);
    }

    boolean[] getAll() {
        //Warning, this is ugly
        boolean[] r = new boolean[8];

        r[0] = get(1);
        r[1] = get(2);
        r[2] = get(3);
        r[3] = get(4);
        r[4] = get(5);
        r[5] = get(6);
        r[6] = get(7);
        r[7] = get(8);

        return r;
    }
}
