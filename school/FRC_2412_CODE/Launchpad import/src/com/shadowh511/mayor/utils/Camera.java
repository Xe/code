/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.utils;

import com.shadowh511.mayor.utils.circleFinder.Frontend;
import edu.wpi.first.wpilibj.camera.*;

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
    void FINDMEATARGET() {
        cam.writeCompression(30);
        targetFinder.findCircle(cam);
    }

    /*
     * Returns Target data
     */
    double[] getATargetRadius(){
        return targetFinder.findCircleSize(cam);
    }

    /*
     * This is a testing function, it wont be used, ignore it
     */
    int howHardDoINeedToKick() {
        rad = getATargetRadius();

        if (rad[0] > 0.15) {
            return 1;
        } else if (rad [0] < 0.15) {
            return 2;
        } else {
            return 0;
        }
    }

    
}
