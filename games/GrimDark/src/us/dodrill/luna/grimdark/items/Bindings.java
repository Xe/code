package us.dodrill.luna.grimdark.items;

import baavgai.textgame.framework.DisplayItem;
import baavgai.textgame.framework.Player;

public class Bindings extends DisplayItem {
	public Bindings() {
		super("bindings");
	}
	
	protected String getLook(Player player) {
		boolean hasPaperclip = player.hasItem("paperclip");
		
		if(hasPaperclip) {
			player.addMessage("There is a keyhole on these bindings, your paperclip looks like it'd undo them. ");
			return "";
		} else {
			player.addMessage("These bindings look like they could be undone with a paperclip, but you don't have one. Too bad, Derpy probably had one. You are sure to die. ");
			return "";
		}
		
	}
	
	
}
