package baavgai.textgame.framework;

public class InventoryItem extends DisplayItem {
	public InventoryItem(String name) { super(name); }
	public InventoryItem(String name, String lookText) { super(name, lookText); }
   
	// assume item can move in and out of inventory
	// depending on state, that may not be the case
	protected boolean canMove(Player player) { return true; }
	
	protected void processTake(Player player) {
		if (canMove(player)) {
			player.moveItemAreaToSelf(this);
		} else {
			player.addMessage("It won't budge.");
		}
	}

	protected void processUse(Player player) {
		player.addMessage("Nothing happens.");
	}
	
	protected void initDefaulActions() {
		super.initDefaulActions();
		actions.add(new ProcessorImpl(this, V_USE) {
			public boolean process(UserAction action, Player player) {
				if (player.hasItem(((InventoryItem)owner))) {
					((InventoryItem)owner).processUse(player);
				}
				return true;
			}
		});
	}
}
