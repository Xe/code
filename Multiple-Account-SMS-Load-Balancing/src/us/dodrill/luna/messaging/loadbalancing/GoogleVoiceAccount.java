package us.dodrill.luna.messaging.loadbalancing;

import com.techventus.server.voice.Voice;
import java.io.IOException;

public class GoogleVoiceAccount implements Messenger {
	private Voice account = null;
	private String email = null;
	private long lastLoginTime = System.currentTimeMillis();
	private int numMessagesSent = 0;
	
	private void reLogin() {
		if(System.currentTimeMillis() - lastLoginTime > 1800000) {
			System.out.println("Need to login to google voice");
			try {
				account.login();
				lastLoginTime = System.currentTimeMillis();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		} 
	}
	
	GoogleVoiceAccount(String u, String p) {
		try {
			account = new Voice(u,p, "MALB", false);
			account.login();
			email = u;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.err.printf("Account %s is bad", u);
			e.printStackTrace();
			
		}
	}
	
	public void sendSMS(String number, String message) throws IOException {
		reLogin();
		try {
			if(number.length() < 10) {
				throw new IOException("Need a longer number");
			} else {
				numMessagesSent++;
				account.sendSMS(number, message);
			}
		} catch (IOException e) {
			System.err.printf("Account %s reported error when trying to send to %s: %s", 
							  email, number, e.getMessage());
			throw new IOException("Account " + email + " found an IOException");
		}
	}
	
	public int getNumMessagesSent() {
		return this.numMessagesSent;
	}
	
	public String toString() {
		return email + ":" + this.numMessagesSent;
	}
	
	public int compareTo(Messenger other) {
		return this.numMessagesSent - other.getNumMessagesSent();
	}
}
