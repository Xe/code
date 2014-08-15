package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Paperclip extends InventoryItem {
	public Paperclip() {
		super("paperclip");
	}
	
	protected void processUse(Player player) {
		if(player.isItemInArea("bindings")) {
			player.addMessage("The paperclip was used to undo the bindings. Don't ask how it works with hooves. \n\n");
			
			player.destroyItemOnSelf("paperclip");
			player.addNewItemToArea(new Gore());
			player.addMessage("\"HEY, WHY ARE YOU DOING THIS? YOUR NUMBER CAME UP, I HAVE NO CHOICE, GET BACK HERE!\"\n\n");
			player.addMessage("You notice that some of the gore that Pinkie had harvested is out, you should grab some.\n");
		}
	}
}
