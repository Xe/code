package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.Joystick;

public class Xbox {
    Joystick joy;

    Xbox(int port) {
        joy = new Joystick(port);
    }

    int a = 1;
    int b = 2;
    int x = 4;
    int y = 3;
    int lb = 6;
    int rb = 5;
    int back = 7;
    int start = 8;

    double[] getAxes(int x_axis, int y_axis) {
        double[] res = new double[2];
        res[0] = joy.getRawAxis(x_axis);
        res[1] = joy.getRawAxis(y_axis);

        return res;
    }

    double[] getLeftStickAxes() {
        return getAxes(1,2);
    }

    double[] getRightStickAxes() {
        return getAxes(4,5);
    }

    boolean getTrigger() {
        return joy.getTrigger();
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

    public Joystick getJoy()
    {
        return joy;
    }
}
