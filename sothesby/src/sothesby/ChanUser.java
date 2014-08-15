package sothesby;

public class ChanUser {
	private Client myClient;
	private Channel myChannel;
	private int prefixes;
	private int cookie; //For mute and ban caching
	
	public ChanUser(Client myClient, Channel myChannel) {
		this(myClient, myChannel, false);
	}
	
	public ChanUser(Client myClient, Channel myChannel, boolean isOp) {
		if (isOp) {
			this.prefixes |= Constants.OPVALUE;
		} else {
			this.prefixes = 0;
		}
		
		this.cookie = myChannel.getCookie();
		this.myClient = myClient;
		this.myChannel = myChannel;
	}

	public int getPrefixes() {
		return prefixes;
	}

	public void setPrefixes(int prefixes) {
		this.prefixes = prefixes;
	}
}
