package sothesby;

import java.util.ArrayList;

public class Channel {
	private final String name;
	private long ts;
	
	private int cookie; //For mute and ban caching
	
	private ArrayList<ChanUser> clients;
	
	public Channel(String chname, long ts) {
		this.name = chname;
		this.setts(ts);
		
		this.clients = new ArrayList<ChanUser>();
	}
	
	public int getCookie() {
		return this.cookie;
	}
	
	public long getts() {
		return ts;
	}

	public void setts(long ts) {
		this.ts = ts;
	}

	public String getName() {
		return this.name;
	}
	
	public void addClient(Client user) {
		this.clients.add(new ChanUser(user, this));
	}
}
