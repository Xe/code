package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Saddle extends InventoryItem {
	
	public Saddle() {
		super("saddle");
	}
	
	protected void processUse(Player player) {
		player.addMessage("You have been wearing it since you took it.\n" + 
			"Nothing happens.");
	}
	
	protected void processTake(Player player) {
		player.addMessage("You put your saddle-bag on, making sure to " + 
			"let your wings through the holes. How you got " + 
			"it on without hands is a mystery.\n");
		super.processTake(player);
		player.addMessage("\n");
	}
	
}
