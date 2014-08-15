/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.util;

import com.shadowh511.mayor.inputs.Xbox;
import com.shadowh511.mayor.inputs.Switch;

/**
 *
 * @author sam
 */

/*
 * This has all of the more advanced functions, like showing the state of the kicker,
 * current speed of the robot, etc.  This class is public while com.shadowh511.mayor.utils.ZomB ain't.
 */

public class EasyZomBDash extends ZomB {
    
    public void xboxGraphs(Xbox input) {
        this.speedGraph(input.getLeftStickX());
        this.graph2(input.getLeftStickY());
    }

    public void xboxAnalog(Xbox input) {
        this.leftAnl((int)Math.floor(this.convertJoyToAnalog(input.getLeftStickX())));
        this.middleAnl((int)Math.floor(this.convertJoyToAnalog(input.getLeftStickY())));
    }

    public void upperSpeedGraph(double input) {
        this.speedGraph(input);
    }

    public void readyLight(boolean input) {
        this.ready(input);
    }

    public void readyLight(Switch input) {
        this.ready(input.ternaryGet());
    }

    public void goLight(boolean input) {
        this.enabledLight(input);
    }

    public void goSwitch(Switch foo){
        this.upperRedLight(foo.ternaryGet());
    }
    
    public double convertJoyToAnalog(double input) {
        //newValue = newMax + (newMax - newMin) * (oldValue- oldMax) / (oldMax - oldMin)
        return 1024 + (1024 - 0) * (input- 1) / (1 - -1);
    }

    public void throttleDisp(double input) {
        input = this.convertJoyToAnalog(input);
        int q = NumberUtils.convertDoubleToInt(input);
        this.middleAnl(q);
    }
}
