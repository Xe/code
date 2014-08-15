package com.shadowh511.mayor.util;

/**
 *
 * @author sam
 */
public class NumberUtils {
    public static int convertDoubleToInt(double input) {
        return (int)input;
    }

    public static double convertIntToDouble(int input) {
        return (double)input;
    }

    public static double rangeConvert(double value, double oldMin, double oldMax, double newMin, double newMax) {
        double newVal = newMax + (newMax - newMin) * (value- oldMax) / (oldMax - oldMin);
        return newVal;
    }

    public static double analogToJoy(double input) {
        return rangeConvert(input, 0, 3.3, 1, -1);
    }

    public static double joyToAnalog(double input) {
        return rangeConvert(input, -1, 1, 0, 3.3);
    }

    public static double joyToServo(double input) {
        return rangeConvert(input, -1, 1, 0, 1);
    }

    public static double servoToJoy(double input) {
        return rangeConvert(input, 0, 1, -1, 1);
    }

    public static int joyToPWM(double input) {
        return convertDoubleToInt(rangeConvert(input, -1, 1, 0, 255));
    }

    public static int joyTo1024Value(double input) {
        return convertDoubleToInt(rangeConvert(input, -1, 1, 0, 1024));
    }

    public static int stringToInt(String input) {
        return Integer.parseInt(input);
    }
}
