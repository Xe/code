package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.*;

public class Picture extends InventoryItem {
	public Picture() {
		super("picture");
	}
	
	protected String getLook(Player player) {
		String msg = "";
		
		msg = msg + "A picture of you and your friends with Princess ";
		msg = msg + "Celestia, you do not want to move it for fear of ";
		msg = msg + "destroying it.  Try taking it if you must.";
		
		return msg;
	}
	
	protected void processTake(Player player) {
		player.addMessage("You stop yourself, your picture is worth a lot to you. " + 
						  "You could not bear to lose it, and the thought makes you " +
						  "shudder. ");
		if(!player.hasItem("note") || player.isItemInArea("note")) {
			player.addNewItemToArea(new Note());
			player.addMessage("Oh look! There's a note on the back! ");
		}
	}
}
