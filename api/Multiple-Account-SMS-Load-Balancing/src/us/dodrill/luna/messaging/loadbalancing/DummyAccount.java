package us.dodrill.luna.messaging.loadbalancing;

import java.io.IOException;

public class DummyAccount implements Messenger {

	private int numMessagesSent;

	public int compareTo(Messenger other) {
		return this.numMessagesSent - other.getNumMessagesSent();
	}

	public int getNumMessagesSent() {
		// TODO Auto-generated method stub
		return this.numMessagesSent;
	}

	public void sendSMS(String number, String message) throws IOException {
		System.out.printf("Message %s sent to %s\n", message, number);
		numMessagesSent++;
	}
	
	public String toString() {
		return ""+this.numMessagesSent;
	}

}
