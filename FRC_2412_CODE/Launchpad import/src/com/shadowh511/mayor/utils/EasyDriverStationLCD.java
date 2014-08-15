package com.shadowh511.mayor.utils;

import edu.wpi.first.wpilibj.DriverStationLCD;

/**
 *
 * @author BenAbraham
 */
public class EasyDriverStationLCD {
    private DriverStationLCD lcd;
    private String[] text = new String[6];
    private int[] offsets = new int[6];

    public boolean clearOnUpdate = true;
    public boolean autoUpdate = true;

    public EasyDriverStationLCD(){
        lcd = DriverStationLCD.getInstance();
        text[0] = "";
        text[1] = "";
        text[2] = "";
        text[3] = "";
        text[4] = "";
        text[5] = "";
        System.out.println("EasyDriverStationLCD initialized");
    }
    public void writeOnLine(int line,String data,int offset){
        text[line] = data;
        offsets[line] = offset;

        if(autoUpdate)
            draw();
    }
    public void draw(){
        if(clearOnUpdate)
            clear();

        //Since the line is an enum we cant loop through
        lcd.println(DriverStationLCD.Line.kMain6, offsets[0]+1, text[0]);
        lcd.println(DriverStationLCD.Line.kUser2, offsets[1]+1, text[1]);
        lcd.println(DriverStationLCD.Line.kUser3, offsets[2]+1, text[2]);
        lcd.println(DriverStationLCD.Line.kUser4, offsets[3]+1, text[3]);
        lcd.println(DriverStationLCD.Line.kUser5, offsets[4]+1, text[4]);
        lcd.println(DriverStationLCD.Line.kUser6, offsets[5]+1, text[5]);
        lcd.updateLCD();
    }
    public void clear(){
        //Clear the screen
        lcd.println(DriverStationLCD.Line.kMain6, 1, "                     ");
        lcd.println(DriverStationLCD.Line.kUser2, 1, "                     ");
        lcd.println(DriverStationLCD.Line.kUser3, 1, "                     ");
        lcd.println(DriverStationLCD.Line.kUser4, 1, "                     ");
        lcd.println(DriverStationLCD.Line.kUser5, 1, "                     ");
        lcd.println(DriverStationLCD.Line.kUser6, 1, "                     ");
    }
}

