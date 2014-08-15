package org.thecatattack.System451.Communication;

import edu.wpi.first.wpilibj.Dashboard;
import edu.wpi.first.wpilibj.DriverStation;

/**
 *
 * @author Patrick Plenefisch
 */
public class ZDashboard {

    String prints = "@@@451:|";
    Dashboard dash = DriverStation.getInstance().getDashboardPackerHigh();

    public ZDashboard() {
    }

    //main
    public void Add(String name, String value) {
        prints += name + "=" + value + "|";
    }

    public void Add(String name, int value) {
        Add(name, String.valueOf(value));
    }

    public void Add(String name, float value) {
        Add(name, String.valueOf(value));
    }

    public void Add(String name, double value) {
        Add(name, String.valueOf(value));
    }

    //main
    public void AddDebugVariable(String name, String value) {
        Add("dbg", name + ": " + value);
    }

    public void AddDebugVariable(String name, int value) {
        AddDebugVariable(name, String.valueOf(value));
    }

    public void AddDebugVariable(String name, float value) {
        AddDebugVariable(name, String.valueOf(value));
    }

    public void AddDebugVariable(String name, double value) {
        AddDebugVariable(name, String.valueOf(value));
    }

   public  void var(String name, String value) {
        AddDebugVariable(name, value);
    }

  public   void var(String name, int value) {
        AddDebugVariable(name, String.valueOf(value));
    }

   public  void var(String name, float value) {
        AddDebugVariable(name, String.valueOf(value));
    }

   public  void var(String name, double value) {
        AddDebugVariable(name, String.valueOf(value));
    }

   public  void Send() {
        dash.addString(prints + ":451@@@");
        dash.commit();
        prints = "@@@451:|";
    }
}