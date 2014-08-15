package com.shadowh511.mayor.outputs;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */



import com.shadowh511.mayor.inputs.*;
import edu.wpi.first.wpilibj.Servo;

/**
 *
 * @author sam
 */
public class JoyServo {
    private Servo foo; //x-axis
    private Servo bar; //y-axis

    public JoyServo (int port1, int port2) {
        this.foo = new Servo(port1);
        this.bar = new Servo(port2);
        System.out.println("JoyServo enabled on ports " + port1 + " and " + port2);
    }

    public double getXAngle() {
        return this.foo.getAngle();
    }

    public double getYAngle() {
        return this.bar.getAngle();
    }

    public double getX() {
        return this.foo.get();
    }

    public double getY() {
        return this.bar.get();
    }

    public void set(double x, double y) {
        this.foo.setAngle(x);
        this.bar.setAngle(y);
    }

    public void set(Xbox controller) {
        double temp;
        this.foo.setAngle(convert(controller.getRightStickX()));
        this.bar.setAngle(convert(controller.getRightStickY()));
    }

    public double convert(double input) {
        //newValue = newMax + (newMax - newMin) * (oldValue- oldMax) / (oldMax - oldMin)
        input = 1 + (1 - 0) * (input- 1) / (1 - -1);
        return input;
    }
}
