package us.dodrill.luna.grimdark.rooms;

import baavgai.textgame.framework.Area;
import baavgai.textgame.framework.Player;

public class RoyalCourt extends Area {
	protected String getLook(Player player) {
		player.addMessage("You report on Pinkie's evils to Princess Celestia. Pinkie is caught and tried ");
		player.addMessage("for the murder of 38 ponies, a new record. You win. You do not hear about her sentence. ");
		player.endGame();
		
		return "";
	}
}
