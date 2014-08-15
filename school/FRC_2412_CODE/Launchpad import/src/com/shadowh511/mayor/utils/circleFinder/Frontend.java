package com.shadowh511.mayor.utils.circleFinder;

/*----------------------------------------------------------------------------*/
/* Copyright (c) FIRST 2008. All Rights Reserved.                             */
/* Open Source Software - may be modified and shared by FRC teams. The code   */
/* must be accompanied by the FIRST BSD license file in the root directory of */
/* the project.                                                               */
/*----------------------------------------------------------------------------*/

import com.shadowh511.mayor.utils.DashBoardVision;
import edu.wpi.first.wpilibj.Timer;
import edu.wpi.first.wpilibj.camera.AxisCamera;
import edu.wpi.first.wpilibj.camera.AxisCameraException;
import edu.wpi.first.wpilibj.image.ColorImage;
import edu.wpi.first.wpilibj.image.NIVisionException;

/**
 * The VM is configured to automatically run this class, and to call the
 * functions corresponding to each mode, as described in the IterativeRobot
 * documentation. If you change the name of this class or the package after
 * creating this project, you must also update the manifest file in the resource
 * directory.
 */
public class Frontend {

    double kScoreThreshold = .01;
    //AxisCamera cam;
    
//    {drive.setInvertedMotor(RobotDrive.MotorType.kRearLeft, true);   drive.setInvertedMotor(RobotDrive.MotorType.kRearRight, true);}
    //Joystick js = new Joystick(1);
    
    DashBoardVision trackerDashboard = new DashBoardVision();

    /**
     * This function is called periodically during autonomous
     */
    public void autonomousPeriodic() {
    }

    /**
     * This function is called while the robot is disabled.
     */
    public void disabledPeriodic() {
    }

    /**
     * This function is called at the beginning of teleop
     */
    public void teleopInit() {
    }
    boolean lastTrigger = false;

    /**
     * This function is called periodically during operator control
     */
    public void findCircle(AxisCamera cam) {
        long startTime = Timer.getUsClock();
            try {
                if (cam.freshImage()) {// && turnController.onTarget()) {
                    ColorImage image = cam.getImage();
                    Thread.yield();
                    Backend[] targets = Backend.findCircularTargets(image);
                    Thread.yield();
                    image.free();
                    if (targets.length == 0 || targets[0].m_score < kScoreThreshold) {
                        System.out.println("No target found");
                        Backend[] newTargets = new Backend[targets.length + 1];
                        newTargets[0] = new Backend();
                        newTargets[0].m_majorRadius = 0;
                        newTargets[0].m_minorRadius = 0;
                        newTargets[0].m_score = 0;
                        for (int i = 0; i < targets.length; i++) {
                            newTargets[i + 1] = targets[i];
                        }
                        trackerDashboard.updateVisionDashboard(0.0, 0.0, 0.0, 0.0, newTargets);
                    } else {
                        System.out.println(targets[0]);
                        System.out.println("Target Angle: " + targets[0].getHorizontalAngle());
                        trackerDashboard.updateVisionDashboard(0.0, 0.0, 0.0, targets[0].m_xPos / targets[0].m_xMax, targets);
                    }
                }
            } catch (NIVisionException ex) {
                ex.printStackTrace();
            } catch (AxisCameraException ex) {
                ex.printStackTrace();
            }
            System.out.println("Time : " + (Timer.getUsClock() - startTime) / 1000000.0);
        }
    public double[] findCircleSize(AxisCamera cam) {
        double angle = 0.0;
        double[]target = new double[7];

        long startTime = Timer.getUsClock();
            try {
                if (cam.freshImage()) {// && turnController.onTarget()) {
                    ColorImage image = cam.getImage();
                    Thread.yield();
                    Backend[] targets = Backend.findCircularTargets(image);
                    Thread.yield();
                    image.free();
                    if (targets.length == 0 || targets[0].m_score < kScoreThreshold) {
                        System.out.println("No target found");
                        Backend[] newTargets = new Backend[targets.length + 1];
                        newTargets[0] = new Backend();
                        newTargets[0].m_majorRadius = 0;
                        newTargets[0].m_minorRadius = 0;
                        newTargets[0].m_score = 0;
                        for (int i = 0; i < targets.length; i++) {
                            newTargets[i + 1] = targets[i];
                        }
                        trackerDashboard.updateVisionDashboard(0.0, 0.0, 0.0, 0.0, newTargets);
                    } else {
                        angle = (targets[0].m_majorRadius);
                        target[0] = targets[0].m_majorRadius;
                        target[1] = targets[0].m_xPos;
                        target[2] = targets[0].m_yPos;
                        System.out.println("Xpos: " + targets[0].m_xPos);
                        System.out.println("Radius: " + angle);
                        trackerDashboard.updateVisionDashboard(0.0, 0.0, 0.0, targets[0].m_xPos / targets[0].m_xMax, targets);
                    }
                }
            } catch (NIVisionException ex) {
                ex.printStackTrace();
            } catch (AxisCameraException ex) {
                ex.printStackTrace();
            }
            
            return target;
        }

}

