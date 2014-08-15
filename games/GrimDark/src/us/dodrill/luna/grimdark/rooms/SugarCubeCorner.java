package us.dodrill.luna.grimdark.rooms;

import us.dodrill.luna.grimdark.items.*;
import baavgai.textgame.framework.*;

public class SugarCubeCorner extends Area {
	boolean firstLook = true;
	
	public SugarCubeCorner(Area prev) {
		this.items.add(new Exit(Exit.DIR_E, prev));
		this.items.add(new PinkiePie());
	}
	
	protected String getLook(Player player) {
		String msg = "";
		
		if(firstLook) {
			msg = msg + "The smell of baked goods overwhelms your senses. This must be none other than the ";
			msg = msg + "home of the Cakes, Ponyville's finest bakers. Pinkie is here, jumping hyper as ever. ";
			msg = msg + "Talk to her using \"talk Pinkie-Pie\" ";
			firstLook = false;
		} else {
			msg = msg + "And suddenly a bakery.  ";
		}
		return msg;
	}
}
