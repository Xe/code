/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.util;

import edu.wpi.first.wpilibj.DriverStation;

/**
 *
 * @author sam
 */
public class EasyDriverStation {
    public DriverStation ds;

    public EasyDriverStation() {
        ds = DriverStation.getInstance();
        System.out.println("EasyDriverStation initiliazed");
    }

    public int getAlliance() {
        DriverStation.Alliance alli = ds.getAlliance();
        return alli.value;
    }
}
