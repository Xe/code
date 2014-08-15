/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package com.shadowh511.mayor.outputs;

import com.shadowh511.mayor.inputs.SideWinder;
import com.shadowh511.mayor.inputs.Attack3;
import com.shadowh511.mayor.inputs.Xbox;
import com.shadowh511.mayor.inputs.DualShock;
import edu.wpi.first.wpilibj.Timer;
import edu.wpi.first.wpilibj.Victor;
import com.sun.squawk.util.MathUtils;

/**
 *
 * @author sam
 */
public class Driver {
    private Victor  FLvictor;
    private Victor  FRvictor;
    private Victor  RLvictor;
    private Victor  RRvictor;
    private boolean mecanum;
    private boolean wheel4;

    /*
     * Usage: Driver robot = new Driver(1,2);
     */
    public Driver(int L, int R) {
        FLvictor = new Victor(L);
        FRvictor = new Victor(R);
        wheel4   = false;
        mecanum  = false;
    }

    /*
     * Usage: Driver robot = new Driver(1,2,3,4);
     */
    public Driver(int FL, int FR, int RL, int RR) {
        wheel4   = true;
        FLvictor = new Victor(FL);
        FRvictor = new Victor(FR);
        RLvictor = new Victor(RL);
        RRvictor = new Victor(RR);
        mecanum  = false;
    }

    /*
     * Usage: Driver robot = new Driver(1,2,3,4,true);
     */
    public Driver(int FL, int FR, int RL, int RR, boolean mecanum) {
        wheel4   = true;
        FLvictor = new Victor(FL);
        FRvictor = new Victor(FR);
        RLvictor = new Victor(RL);
        RRvictor = new Victor(RR);
        mecanum  = true;
    }

    /*
     * This function takes in one input, speed.  If you dont have the mecanum
     * flag defined, it will skip to just setting the speed.  Otherwise, it will
     * set the speeds of the first two Victors.  If you are using four wheels
     * in the constructor, it will also set those speeds accordingly.
     */
    public void drive(double speed) {
        if (!mecanum) {
            FLvictor.set(speed);
            FRvictor.set(-speed);

            if (wheel4) {
                RLvictor.set(speed);
                RRvictor.set(-speed);
            }
        } else {
            this.checkingDrive(speed, 0);
        }
    }

    /*
     * This functon takes in two inputs, speed and curve.  It then creates
     * temporary speed variables.  It borrows WPI's curve calculation algorithm
     * to creatively figure out the individual motor values.  It then calls
     * tankDrive to set the motor speeds.
     */
    public void drive(double speed, double curve) {
        double leftSpeed, rightSpeed;

        if (curve < 0) {    // borrowed from edu.wpi.first.wpilibj.RobotDrive, calculates the curve
            double value = MathUtils.log(-curve);
            double ratio = (value - 0) / (value + 0);

            if (ratio == 0) {
                ratio = .0000000001;
            }

            leftSpeed  = speed / ratio;
            rightSpeed = speed;
        } else if (curve > 0) {
            double value = MathUtils.log(curve);
            double ratio = (value - 0) / (value + 0);

            if (ratio == 0) {
                ratio = .0000000001;
            }

            leftSpeed  = speed;
            rightSpeed = speed / ratio;
        } else {
            leftSpeed  = speed;
            rightSpeed = speed;
        }                   // end borrowing

        this.tankDrive(leftSpeed, rightSpeed);    // yay for brilliant hacks!!!
    }

    public void drive(double speed, double curve, double delay) {
        this.drive(speed, curve);
        Timer.delay(delay);
    }

    /*
     * ArcadeDrive, manual version
     */
    public void arcadeDrive(double speed, double curve) {
        this.drive(speed, curve);
    }

    /*
     * ArcadeDrive, xbox360 controller version
     */
    public void arcadeDrive(Xbox foo) {
        this.drive(foo.getLeftStickY(), foo.getLeftStickX());
    }

    /*
     * ArcadeDrive, SideWinder version
     */
    public void arcadeDrive(SideWinder foo) {
        this.drive(foo.getYAxis(), foo.getXAxis());
    }

    /*
     * This takes two inputs, left speed and right speed
     * it sets the first two victors
     * then it checks if you called Driver with 4 motor ports
     * if that is true, it sets the values for the back two motors
     */
    public void tankDrive(double left, double right) {
        if (!mecanum) {
            FLvictor.set(left);
            FRvictor.set(-right);

            if (wheel4) {
                RLvictor.set(left);
                RRvictor.set(-right);
            }
        }
    }

    /*
     * TankDrive, xbox 360 input version
     */
    public void tankDrive(Xbox foo) {
        this.tankDrive(foo.getLeftStickY(), foo.getRightStickY());
    }

    /*
     * TankDrive for 2 sideWinders
     */
    public void tankDrive(SideWinder foo, SideWinder bar) {
        this.tankDrive(foo.getYAxis(), bar.getYAxis());
    }

    /*
     * Attack3 joystick version
     */
    public void tankDrive(Attack3 foo, Attack3 bar) {
        this.tankDrive(foo.getY(), bar.getY());
    }

    /*
     * DualShock version (ps2 controller)
     */
    public void tankDrive(DualShock foo) {
        this.tankDrive(foo.getLeftStickY(), foo.getRightStickY());
    }

    /*
     * A nice interface for the users of 3D joysticks
     */
    public void mecanumDrive(SideWinder foo) {
        this.checkingDrive(foo.getYAxis(), foo.getXAxis());
        this.turn(foo.getTwistAxis());
    }

    /*
     * Another nice interface for those who use a PS2 DualShock controller
     */
    public void mecanumDrive(DualShock foo) {
        this.checkingDrive(foo.getLeftStickY(), foo.getLeftStickX());
        this.turn(foo.getRightStickX());
    }

    /*
     * Yet another nice interface for people who use an xBox 360 controller
     */
    public void mecanumDrive(Xbox foo) {
        double power, rotation, strafe;

        power = foo.getLeftStickY();
        rotation = foo.getLeftStickX();
        strafe = foo.getRightStickX();

        this.checkingDrive(power, rotation);
        this.turn(strafe);
    }

    /*
     * Our hack to turn our mecanum wheels, call it separately
     */
    public void turn(double power) {
        if (Math.abs(power) > 0) {
            RRvictor.set(power);
            RLvictor.set(power);
        }
    }

    /*
     * Makes sure we dont send a n<1 value to the victors, they dont like that
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

    /*
     * This brilliant hack was make by our team's all around awesome mentor, Dimitri
     * Dont touch it
     *
     * Takes one input, input.  if the input is positive, it squares it.  If its
     * negative, it squares it while preserving the sign.  It returns the input.
     */
    double fiddleWithSpeed(double input) {
        if (input > 0) {
            input = input * input;
        } else {
            input = -input * input;
        }

        return input;
    }

    public void checkingDrive(double magnitude, double rotation) { //DONT TOUCH THIS
        magnitude = limit(magnitude);  //1 is highest, so make n > 1, 1
        rotation  = limit(rotation);

        double frontLeftSpeed, rearLeftSpeed, frontRightSpeed, rearRightSpeed;  //temporary motor speed values

        frontLeftSpeed = ((magnitude - rotation));  //calculate speed values
        frontRightSpeed = ((magnitude + rotation));
        rearLeftSpeed = ((magnitude + rotation));
        rearRightSpeed = ((magnitude - rotation));

        
        double maxMotor = Math.max(
            Math.max(Math.abs(frontLeftSpeed), Math.abs(frontRightSpeed)),
            Math.max(Math.abs(rearLeftSpeed), Math.abs(rearRightSpeed))
        );

        if (maxMotor > 1){
            frontRightSpeed = frontRightSpeed / maxMotor;
            rearRightSpeed = rearRightSpeed / maxMotor;
            frontLeftSpeed = frontLeftSpeed / maxMotor;
            rearLeftSpeed = rearLeftSpeed / maxMotor;
        }
        

        frontLeftSpeed = limit(fiddleWithSpeed(frontLeftSpeed));
        frontRightSpeed = limit(fiddleWithSpeed(frontRightSpeed));
        rearLeftSpeed = limit(fiddleWithSpeed(rearLeftSpeed));
        rearRightSpeed = limit(fiddleWithSpeed(rearRightSpeed));

        if(magnitude != 0 || rotation !=0) {
            System.out.println("#############################################");
            System.out.println("Mangnitude: " + magnitude + "  Rotation: " + rotation);  //debugOut
            System.out.println("FLSpeed: " + frontLeftSpeed + "  FRSpeed: " + frontRightSpeed);
            System.out.println("RLSpeed: " + rearLeftSpeed + "  RRSpeed: " + rearRightSpeed);
        }

        FLvictor.set(-frontLeftSpeed);  //set the speeds on the motors
        FRvictor.set(frontRightSpeed);
        RLvictor.set(-rearLeftSpeed);
        RRvictor.set(rearRightSpeed);
    }

    public void stop() {
        this.drive(0);
    }

}