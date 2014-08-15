package com.robototes.abomasnow;

import edu.wpi.first.wpilibj.*;

public class Driver {
    private Jaguar  FLjaguar;
    private Jaguar  FRjaguar;
    private Jaguar  RLjaguar;
    private Jaguar  RRjaguar;
    private boolean mecanum;
    private boolean wheel4;

    /*
     * Usage: Driver robot = new Driver(1,2);
     */
    public Driver(int L, int R) {
        FLjaguar = new Jaguar(L);
        FRjaguar = new Jaguar(R);
        wheel4   = false;
        mecanum  = false;
    }

    /*
     * Usage: Driver robot = new Driver(1,2,3,4);
     */
    public Driver(int FL, int FR, int RL, int RR) {
        wheel4   = true;
        FLjaguar = new Jaguar(FL);
        FRjaguar = new Jaguar(FR);
        RLjaguar = new Jaguar(RL);
        RRjaguar = new Jaguar(RR);
        mecanum  = false;
    }

    /*
     * Usage: Driver robot = new Driver(1,2,3,4,true);
     */
    public Driver(int FL, int FR, int RL, int RR, boolean mecanum) {
        wheel4   = true;
        FLjaguar = new Jaguar(FL);
        FRjaguar = new Jaguar(FR);
        RLjaguar = new Jaguar(RL);
        RRjaguar = new Jaguar(RR);
        this.mecanum = true;
    }

    Driver() {  //for autonomousDriver, dont use
        FLjaguar = new Jaguar(1);
        FRjaguar = new Jaguar(2);
        RLjaguar = new Jaguar(3);
        RRjaguar = new Jaguar(4);
        wheel4 = true;
        mecanum = true;
    }

    /*
     * This function takes in one input, speed.  If you dont have the mecanum
     * flag defined, it will skip to just setting the speed.  Otherwise, it will
     * set the speeds of the first two Jaguars.  If you are using four wheels
     * in the constructor, it will also set those speeds accordingly.
     */
    public void drive(double speed) {
        if (!mecanum) {
            FLjaguar.set(speed);
            FRjaguar.set(-speed);

            if (wheel4) {
                RLjaguar.set(speed);
                RRjaguar.set(-speed);
            }
        } else {
            this.holonomicDrive(speed, 0, 0);
        }
    }

    /*
     * This takes two inputs, left speed and right speed
     * it sets the first two jaguars
     * then it checks if you called Driver with 4 motor ports
     * if that is true, it sets the values for the back two motors
     */
    public void tankDrive(double left, double right) {
        if (!mecanum) {
            FLjaguar.set(left);
            FRjaguar.set(-right);

            if (wheel4) {
                RLjaguar.set(left);
                RRjaguar.set(-right);
            }
        }
    }

    /*
     * TankDrive, xbox 360 input version
     */
    public void tankDrive(Xbox foo) {
        this.tankDrive(foo.getLeftStickAxes()[1], foo.getRightStickAxes()[1]);
    }

    /*
     * Makes sure we dont send a n<1 value to the jaguars, they dont like that
     */
    double limit(double num) {
        if (num > 1.0) {
            return 1.0;
        }

        if (num < -1.0) {
            return -1.0;
        }

        return num;
    }

    double maxOf4(double one, double two, double three, double four) {
        return Math.max(Math.max(Math.abs(one), Math.abs(two)), Math.max(Math.abs(three), Math.abs(four)));
    }

    /*
     * This brilliant hack was make by our team's all around awesome mentor, Dimitri
     * Dont touch it
     *
     * Takes one input, input.  if the input is positive, it squares it.  If its
     * negative, it squares it while preserving the sign.  It returns the input.
     *
     * It allows greater control of speed while preserving the ability to go at full
     * speed
     */
    public double fiddleWithSpeed(double input) {
        if (input > 0) {
            input = input * input;
        } else {
            input = -input * input;
        }

        return input;
    }

    public void holonomicDrive(double power, double slide, double spin) {
        /*
         * This is a correct mecanum wheel drive function
         */
        double fl, fr, rl, rr;

        fl = power + slide + spin;
        fr = power - slide - spin;
        rl = power - slide + spin;
        rr = power + slide - spin;

        double max = this.maxOf4(fl, fr, rl, rr);

        if (max>1) {
            fl = fl/max;
            fr = fr/max;
            rl = rl/max;
            rr = rr/max;
        }

        fl = limit(/*fiddleWithSpeed*/(fl));
        fr = limit(/*fiddleWithSpeed*/(fr));
        rl = limit(/*fiddleWithSpeed*/(rl));
        rr = limit(/*fiddleWithSpeed*/(rr));

        if(power != 0 || slide !=0 || spin != 0) {
            System.out.println("#############################################");
            System.out.println("power: " + power + "  spin: " + spin + "  slide: " + slide);  //debugOut
            System.out.println("FLSpeed: " + fl + "  FRSpeed: " + fr);
            System.out.println("RLSpeed: " + rl + "  RRSpeed: " + rr);
        }

        this.set(fl, fr, rl, rr);
    }

    public void hackedDrive(double power, double turn) {
        double left,right;

        left = power - turn;
        right = power + turn;

        double max = Math.max(left, right);

        if (max > 1) {
            left = left/max;
            right = right/max;
        }

        left = limit(fiddleWithSpeed(left));
        right = limit(fiddleWithSpeed(right));

        this.set(left, right);
    }

    public void stop() {
        this.drive(0);
    }

    public void set(double left, double right) {
        this.tankDrive(left, right);
    }

    public void set(double FL, double FR, double RL, double RR) {
        this.FLjaguar.set((limit(FL)));
        this.FRjaguar.set((limit(-FR)));
        this.RLjaguar.set(((limit(RL))));
        this.RRjaguar.set((limit(-RR)));
    }

    public double getAVGSpeed() {
    /*
     * If there are 4 wheels defined, return the values of all the jaguar values averaged, otherwise,
     * return the speeds of the two wheels averaged.
     *
     * I know its ternary abuse, but it works (i tested it in javascript)
     */
        return wheel4 //condition to test
                ? (this.FLjaguar.get() + this.FRjaguar.get() + //if its true
                    this.RLjaguar.get() + this.RRjaguar.get())/ 4 //cont.
                : (this.FLjaguar.get() + this.FRjaguar.get()) / 2; //if its false
    }

    public void strafeLeft() {
        this.holonomicDrive(0, 0.5, 0);
    }

    public void strafeRight() {
        this.holonomicDrive(0, -0.5, 0);
    }

    public void goForward() {
        this.holonomicDrive(0.5, 0, 0);
    }

    public void goBackward() {
        this.holonomicDrive(-0.5, 0, 0);
    }

    public void turnRight() {
        this.holonomicDrive(0, 0, 0.5);
    }

    public void turnLeft() {
        this.holonomicDrive(0, 0, -0.5);
    }
}