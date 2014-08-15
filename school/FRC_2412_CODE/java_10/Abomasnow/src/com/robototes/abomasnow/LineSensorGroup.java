package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.DigitalInput;

/**
 *
 * @author sam
 */
public class LineSensorGroup {
    DigitalInput left;
    DigitalInput middle;
    DigitalInput right;

    LineSensorGroup(int l, int m, int r) {
        left = new DigitalInput(l);
        middle = new DigitalInput(m);
        right = new DigitalInput(r);
    }

    boolean[] get(){
        boolean[] r = {left.get(), middle.get(), right.get()};
        return r;
    }

    boolean getLeft() {
        return get()[0];
    }

    boolean getMiddle() {
        return get()[1];
    }

    boolean getRight() {
        return get()[2];
    }

    int[] getNumbers() {
        int[] r = {left.get()?1:0,middle.get()?1:0,right.get()?1:0};
        return r;
    }
    boolean allAreOff() {
        boolean r = false;

        r = (this.getLeft() == this.getMiddle() == this.getRight() == false) ?
            true : false;
        return r;
    }
    boolean allAreOn() {
        boolean r = false;

        r = (this.getLeft() == this.getMiddle() == this.getRight() == true) ?
            true : false;
        return r;
    }
}
