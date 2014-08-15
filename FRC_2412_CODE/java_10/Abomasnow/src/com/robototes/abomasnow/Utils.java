package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.Compressor;
import edu.wpi.first.wpilibj.camera.AxisCamera;

public class Utils {
    static Compressor comp;
    static AxisCamera cam;

    public static void put(String n){
        System.out.println(n);
    }

    public static void initComp() {
        comp = new Compressor(1,1);
        comp.start();
    }

    public static void initCam() {
        cam = AxisCamera.getInstance();
    }
}
