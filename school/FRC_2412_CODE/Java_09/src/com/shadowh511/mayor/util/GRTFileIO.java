/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.shadowh511.mayor.util;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import javax.microedition.io.Connector;
import com.sun.squawk.io.BufferedReader;
import com.sun.squawk.microedition.io.FileConnection;

/*
 * I copied this code from the GRT Framework
 */

public class GRTFileIO {
	public static String getFileContents(String filename) {
		String url = "file:///" + filename;
		String contents = "";
		try {
			FileConnection c = (FileConnection) Connector.open(url);
			BufferedReader buf = new BufferedReader(new InputStreamReader(c
					.openInputStream()));
			String line = "";
			while ((line = buf.readLine()) != null) {
				contents += line + "\n";
			}
			c.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return contents;
	}

	public static void writeToFile(String filename, String contents) {
		String url = "file:///" + filename;
		try {
			FileConnection c = (FileConnection) Connector.open(url);
			OutputStreamWriter writer = new OutputStreamWriter(c
					.openOutputStream());
			writer.write(contents);
			c.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}