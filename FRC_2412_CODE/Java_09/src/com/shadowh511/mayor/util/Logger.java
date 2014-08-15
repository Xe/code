/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.util;

/**
 *
 * @author sam
 */
public class Logger {
    private String log = "";
    private String name = "latest.log";

    public Logger(){}

    public Logger(String args) {
        name = args;
    }


    public void append(String data) {
        log = log + data + "\n";
        System.out.println(data);
    }

    public void commit() {
        System.out.println("Log written to /" + name);
        GRTFileIO.writeToFile(name, log);
    }
}
