package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.DigitalInput;

public class Switch {
    private DigitalInput source;

    public Switch(int channel) {
        this.source = new DigitalInput(4,channel);
    }

    public Switch(int slot, int channel) {
        this.source = new DigitalInput(slot,channel);
    }

    public boolean oldGet() {
        return this.source.get();
    }

    public boolean get() {
        if(!this.source.get()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean ternaryGet() {
        return this.source.get() ? false : true;
        /*
         * If this.source.get() is true, it is really a false value because it
         * is always true, unless the switch is pressed, then the value is false
         *
         * This is an easier way to write the this.get() function
         */
    }
}
