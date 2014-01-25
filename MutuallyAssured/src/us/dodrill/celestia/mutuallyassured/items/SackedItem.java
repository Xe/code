package us.dodrill.celestia.mutuallyassured.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class SackedItem extends InventoryItem {
	public SackedItem(String name) {
		super(name);
	}
	
	public SackedItem(String name, String desc) {
		super(name, desc);
	}
	
	protected void processTake(Player player) {
		if(player.hasItem("sack")) {
			super.processTake(player);
		} else {
			player.addMessage("You need somewhere to put it, maybe the sack would do?\n");
		}
	}
}
