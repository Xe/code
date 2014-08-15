/*
 * This class is not public, it is not a bug
 */

package com.shadowh511.mayor.util;

import org.thecatattack.System451.Communication.ZDashboard;

/**
 *
 * @author sam
 */
class ZomB {
    private ZDashboard ZomB = new ZDashboard();
    private boolean kill = false;

    public void speedGraph (double value) {
        ZomB.Add("speedgraph", value);
    }

    public void graph1 (double value) {
        ZomB.Add("grph", value);
    }

    public void graph2 (double value) {
        ZomB.Add("grph2", value);
    }

    public void taco (double value) {
        ZomB.Add("taco", value);
    }

    public void leftMeter (double value) {
        ZomB.Add("left", value);
    }

    public void rightMeter (double value) {
        ZomB.Add("right", value);
    }

    public void leftAnl (int value) {
        ZomB.Add("joy1s1x", value);
    }

    public void middleAnl (int value) {
        ZomB.Add("joy1s1y", value);
    }

    public void rightAnl (int value) {
        ZomB.Add("joy1s2x", value);
    }

    public void batMet (double value) {
        ZomB.Add("bat", value);
    }

    public void servo1 (double value) {
        ZomB.Add("servo", value);
    }

    public void servo2 (double value) {
        ZomB.Add("servo1", value);
    }

    public void leftDirMeter (int value) {
        ZomB.Add("dir", value);
    }

    public void rightDirMeter (int value) {
        ZomB.Add("dir2", value);
    }

    public void turn (double value) {
        ZomB.Add("turn", value);
    }

    public void enabledLight(boolean value) {
        ZomB.Add("GO", value ? 1 : 0);
    }

    public void upperRedLight(boolean value) {
        ZomB.Add("sw1", value ? 1 : 0);
    }

    public void ready (boolean value) {
        ZomB.Add("ready", value ? 1 : 0);
    }

    public void lowerRedLight(boolean value) {
        ZomB.Add("sw2", value ? 1 : 0);
    }

    public void upperSpikeMeter(int value) {
        ZomB.Add("spk1", value);
    }

    public void lowerSpikeMeter(int value) {
        ZomB.Add("spk2", value);
    }
    
    public void log(String data) {
        ZomB.Add("log", data);
    }

    public void update() {
        ZomB.Send();
    }
}
