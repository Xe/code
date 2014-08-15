package us.dodrill.luna.grimdark.items;

import us.dodrill.luna.grimdark.rooms.Ponyville;
import us.dodrill.luna.grimdark.rooms.StartRoom;
import baavgai.textgame.framework.AreaImpl;
import baavgai.textgame.framework.Exit;
import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Gore extends InventoryItem {
	public Gore() {
		super("gore");
	}
	
	protected String getLook(Player player) {
		player.addMessage("Some body parts of other ponies. Should make for enough evidence for the princess.");
		
		return "";
	}
	
	protected void processTake(Player player) {
		if(player.hasItem("saddle")) {
			player.addMessage("You scoop the gore into your saddle, putting it in the bag. You will need to buy a new one later. You see an exit to the south appear. ");
			super.processTake(player);
			player.addNewItemToArea(new Exit(Exit.DIR_S, new AreaImpl("") {
				public void processLook(Player player) {
					player.addMessage("You see a way out, you instantly take it.\n\n");
					player.moveTo(new Ponyville(new StartRoom()));
				}
			}));
		} else {
			player.addMessage("you have nothing to hold the evidence in."); 
		}
	}
}
