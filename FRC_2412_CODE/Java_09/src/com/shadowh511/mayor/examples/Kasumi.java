package com.shadowh511.mayor.examples;

import com.shadowh511.mayor.outputs.*;
import com.shadowh511.mayor.inputs.Switch;
import com.shadowh511.mayor.util.Camera;
import com.shadowh511.mayor.util.EasyDriverStation;
import com.shadowh511.mayor.util.EasyDriverStationLCD;
import com.shadowh511.mayor.util.Logger;
import edu.wpi.first.wpilibj.SimpleRobot;
import edu.wpi.first.wpilibj.Watchdog;

public class Kasumi extends SimpleRobot {
    Driver robot;
    Firing gun;
    Camera cam = new Camera();
    Logger lobster = new Logger("CameraAutonomous");
    Switch balls;
    EasyDriverStation ds = new EasyDriverStation();
    EasyDriverStationLCD lcd = new EasyDriverStationLCD();
    Watchdog q = this.getWatchdog();

    public Kasumi(Driver in, Firing theirs, Switch theirss){
        robot = in;
        gun = theirs;
        balls = theirss;
        robot.stop();
        lobster.append("Started up Kasumi");
    }

    public void autonomous() {
        q.setEnabled(true);
        q.feed();
        lobster.append("Starting Autonomous Function");

        while (this.isEnabled() && this.isAutonomous()) {
            
        }
    }
}