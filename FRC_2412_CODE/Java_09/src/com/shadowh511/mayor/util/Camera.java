/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.util;

import com.shadowh511.mayor.util.circleFinder.Frontend;
import edu.wpi.first.wpilibj.camera.*;
import edu.wpi.first.wpilibj.Timer;

/**
 *
 * @author sam
 */
public class Camera {
    private AxisCamera cam = AxisCamera.getInstance();
    private DashBoardVision dash = new DashBoardVision();
    private Frontend targetFinder = new Frontend();
    
    private double[] rad = new double[7];

    public Camera() {
        System.out.println("Camera is enabled");
    }

    /*
     * Displays the target on the dashboard
     */
    public void FINDMEATARGET() {
        cam.writeCompression(30);
        targetFinder.findCircle(cam);
    }

    /*
     * Returns Target data
     */
    public double[] getATargetRadius(){
        return targetFinder.findCircleSize(cam);
    }

    /*
     * This is a testing function, it wont be used, ignore it
     */
    public int howHardDoINeedToKick() {
        rad = getATargetRadius();

        if (rad[0] > 0.15) {
            return Constants.Firing.soft;
        } else if (rad [0] < 0.15) {
            return Constants.Firing.hard;
        } else {
            return 0;
        }
    }
    
}
