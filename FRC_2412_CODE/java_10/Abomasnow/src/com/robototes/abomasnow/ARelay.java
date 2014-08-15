package com.robototes.abomasnow;
/*
 * Abomasnow, the Abomnidable pokemon, grass-ice type
 * Alphabetically the first pokemon in the pokedex (4th gen)
 */

import edu.wpi.first.wpilibj.Relay;
import edu.wpi.first.wpilibj.Timer;

public class ARelay {
    Relay dev;

    ARelay(int n){
        dev = new Relay(n);
    }

    void on(){
        dev.set(Relay.Value.kOn);
    }

    void off(){
        dev.set(Relay.Value.kOff);
    }

    void forward(){
        dev.set(Relay.Value.kForward);
    }

    void reverse(){
        dev.set(Relay.Value.kReverse);
    }

    void fire(boolean condition, Driver robot) {
        if(condition) {
            robot.stop();
            on();
            Timer.delay(0.125);
            off();
        }
    }
}
