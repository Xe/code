package baavgai.textgame.framework;

public class AreaImpl extends Area {
	private final String message;
	private final String firstMessage;
	public AreaImpl(String message, String firstMessage) {
		super();
		this.message = message;
		this.firstMessage = firstMessage;
	}
	
	public AreaImpl(String message) { this(message, null); }
	
	protected String getLook(Player player) {
		if (this.visits==1 && firstMessage!=null) {
			return firstMessage;
		} else {
			return message;
		}
	}
	
}
