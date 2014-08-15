package us.dodrill.luna.messaging.loadbalancing;

import java.util.*;
import java.io.*;

public class Pool {
	ArrayList<Messenger> accounts = new ArrayList<Messenger>();
	private final int BEST_ACCOUNT = 0;
	
	public Pool() {
		System.err.println("New Pool created");
	}
	
	public void addGV(String uname, String password) {
		GoogleVoiceAccount toAdd = new GoogleVoiceAccount(uname, password);
		
		accounts.add(toAdd);
	}
	
	public void addDummy() {
		DummyAccount toAdd = new DummyAccount();
		
		accounts.add(toAdd);
	}
	
	public void add(Messenger m) {
		accounts.add(m);
	}
	
	private void selectBestAccount() {
		Collections.sort(accounts);
	}
	
	public boolean sendSMS(String number, String message) {
		selectBestAccount();
		
		try {
			Messenger sender = accounts.get(BEST_ACCOUNT);
			
			sender.sendSMS(number, message);
			
			return true;
		} catch (IndexOutOfBoundsException e) {
			System.err.println("You need at least one account.");
			return false;
		} catch (IOException e) {
			System.err.println("Something below is not happy");
			System.err.println(e.getMessage());
			e.printStackTrace();
			return false;
		}
	}
}
