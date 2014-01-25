package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.InventoryItem;
import baavgai.textgame.framework.Player;

public class Note extends InventoryItem {
	public Note() {
		super("note");
	}
	
	public void processLook(Player player) {
		player.addMessage("Hello player!\n\nThis is GrimDark, the Text-based adventure game by shadowh511.\n\n");
		player.addMessage("The scenario is a lot like the MLP:FIM fanfiction Cupcakes.  I do not own this scenario or any of the characters.\n\n");
		player.addMessage("This game is pretty simple, so have fun!\n\n");
		player.addMessage("If you have any requests or want to file any bugs, email me at shadow.h511@gmail.com, I will get back to you quickly.\n\n");
		player.addMessage("\"As long as you maintain your spirit of adventure, I am sure you will make it out\"\n");
	}
}
