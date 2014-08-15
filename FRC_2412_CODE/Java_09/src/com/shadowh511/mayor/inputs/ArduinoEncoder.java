package com.shadowh511.mayor.inputs;

import com.shadowh511.mayor.util.Arduino;
import com.shadowh511.mayor.util.NumberUtils;

/**
 *
 * @author sam
 *
 * This hopefully allows the cRIO to use an arduino as an input for the robot
 * over the friendly, neighborhood serial protocol.
 */
public class ArduinoEncoder {
    private String direction;
    ArduinoEncoder.WorkerThread slave = new ArduinoEncoder.WorkerThread();
    
    public ArduinoEncoder(String axis) {
        direction = axis;
        System.out.println("Using the " + axis + " axis");
        slave.start();
    }

    public int get() {
        return slave.get();
    }

    private class WorkerThread extends Thread {
        private Arduino source = new Arduino();
        int count;

        public void run() {
            try {
                this.wait(); //Wait until stuff happens
                this.count();
            } catch (Exception e) {
                System.out.println("ArduinoEncoder.WorkerThread: " + e.getMessage());
            }
        }

        private void count() {
            this.count = this.instantData();
        }

        private int get() {
            return this.count;
        }

        private int instantData() {
            return NumberUtils.stringToInt(this.source.requestData());
        }
    }
}
