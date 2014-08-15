package com.shadowh511.mayor.util;

import java.util.Stack;
/**
 *
 * @author sam
 */
public class Scripting {
    String file;
    Logger log = new Logger("Scripting.log");
    Stack instructions = new Stack();
    char[] results = new char[9999];

    public Scripting(String filename) {
        file = GRTFileIO.getFileContents("\\" + filename);
        log.append("opened " + filename);
    }

    public void parse() {
        int cur = 0;
        int lastWord = 0;
    }
}
