package baavgai.textgame.framework;

public class DisplayItem extends ItemBase {
	public DisplayItem(String name) { super(name); }
	public DisplayItem(String name, String lookText) { super(name, lookText); }
	
	protected void processTake(Player player) {
		player.addMessage("It won't budge.");
	}

	protected void initDefaulActions() {
		super.initDefaulActions();
		actions.add(new ProcessorImpl(this, V_TAKE) {
			public boolean process(UserAction action, Player player) {
				((DisplayItem)owner).processTake(player);
				return true;
			}
		});
	}
	
}
