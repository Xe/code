package us.dodrill.celestia.mutuallyassured.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Sack extends InventoryItem {
	public Sack() {
		super("sack", "You don’t have pockets, or clothes, so you need this bag to store things. It has enough space for what you need to store.");
	}
	
	protected void processTake(Player player) {
		player.addMessage("Yay, you sling the sack over your shoulders and find a comfortable spot for it to rest. It lands between your wings. Oh yeah, you have wings, and you are colored cyan. Just thought you should know. ");
		super.processTake(player);
	}
}
