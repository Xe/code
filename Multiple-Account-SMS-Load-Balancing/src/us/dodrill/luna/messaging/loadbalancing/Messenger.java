package us.dodrill.luna.messaging.loadbalancing;

import java.io.IOException;

public interface Messenger extends Comparable<Messenger> {
	public int getNumMessagesSent();
	public void sendSMS(String number, String message) throws IOException;
	public String toString();
}