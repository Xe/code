package us.dodrill.luna.grimdark.rooms;

import us.dodrill.luna.grimdark.items.Paperclip;
import baavgai.textgame.framework.*;

public class Ponyville extends Area {
	boolean firstLook = true;
	boolean paperClipOut = false;
	
	public Ponyville(Area prev) {
		this.items.add(new Exit(Exit.DIR_N, prev));
		this.items.add(new Exit(Exit.DIR_W, new SugarCubeCorner(this)));
	}
	
	protected String getLook(Player player) {
		String msg = "";
		
		if(firstLook && !player.hasItem("gore")) {
			msg = msg + "You remember that you need to go to Sugarcube Corner, as that is where Pinkie ";
			msg = msg + "is waiting for you. It is to the west. You see Derpy fly by, she's probably off ";
			msg = msg + "delivering the mail again.  It is a happy day, almost too perfect.  ";
			firstLook = false;
		} else if(!player.hasItem("gore")) {
			msg = msg + "It's a happy day. Not much else to be said.  ";
			if(!paperClipOut){
				msg = msg + "Oh hey! A paperclip! Derpy must have dropped it! ";
				this.items.add(new Paperclip());
				paperClipOut = true;
			}
		} else {
			msg = msg + "It's Ponyville, you are relieved. You remember Canterlot is to the east. You must go there.\n";
			this.items.add(new Exit(Exit.DIR_E, new RoyalCourt()));
		}
		return msg;
	}
}
