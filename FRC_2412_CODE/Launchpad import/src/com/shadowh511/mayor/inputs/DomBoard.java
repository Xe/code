/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.inputs;

import edu.wpi.first.wpilibj.DriverStation;

/**
 *
 * @author sam
 */
public class DomBoard {
    private DriverStation ds = DriverStation.getInstance();
    public boolean[] inputs = new boolean[8];

    public DomBoard() {
        System.out.println("DomBoard Initialized");
    }

    public void populateInputs() {
        int i = 8;

        while (i>-1) {
            inputs[i] = ds.getDigitalIn(i);
            i--;
        }
    }

    public boolean[] getInputs() {
        boolean[] in = new boolean[8];

        int i = 8;

        while (i>-1) {
            in[i] = ds.getDigitalIn(i);
            i--;
        }

        return in;
    }

    public void setOutput (int channel, boolean value) {
        ds.setDigitalOut(channel, value);
    }

    public void setOutput (int channel) {
        boolean temp;

        temp = ds.getDigitalOut(channel);

        if(temp) {
            this.setOutput(channel, false);
        } else {
            this.setOutput(channel, true);
        }
    }

    public boolean getLeftSwitch() {
        this.populateInputs();
        return inputs[2];
    }

    public boolean getRightSwitch() {
        this.populateInputs();
        return this.inputs[4];
    }

    public boolean getLeftButton() {
        this.populateInputs();
        return this.inputs[6];
    }

    public boolean getRightButton() {
        this.populateInputs();
        return this.inputs[8];
    }

    public boolean getUpperButton() {
        this.populateInputs();
        return this.inputs[5];
    }

    public void setLed(int channel, boolean value) {
        this.setOutput(channel, value);
    }

    public int getAllianceSwitches() {
        if(getRightSwitch() && getLeftSwitch()) {
            return 3;
        } else if(getRightSwitch()) {
            return 2;
        } else if(getLeftSwitch()) {
            return 1;
        } else {
            return 0;
        }
    }
}
