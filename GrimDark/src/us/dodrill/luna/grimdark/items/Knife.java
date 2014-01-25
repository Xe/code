package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Knife extends InventoryItem {

	public Knife() {
		super("knife");
	}
	
	protected void processLook(Player player) {
		player.addMessage("Pinkie's knife.  It has a coat of blood.  It is now yours as evidence.  ");
	}
}
