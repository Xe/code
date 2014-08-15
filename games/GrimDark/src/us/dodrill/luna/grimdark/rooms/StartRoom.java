package us.dodrill.luna.grimdark.rooms;

import baavgai.textgame.framework.*;
import us.dodrill.luna.grimdark.items.*;

public class StartRoom extends Area {
	public StartRoom() {
		//this.lookText = this.getLook();
		
		this.items.add(new Saddle());
		this.items.add(new Picture());
		
		this.items.add(new Exit(Exit.DIR_S, new Ponyville(this)));
	}
	
	protected String getLook(Player player) {
		String msg = "";
		
		msg = msg + "You are in your room, standing next to your bed. You remember that you need ";
		msg = msg + "to meet up with Pinkie Pie.  You should remember to put on your saddle, it might be useful later.";
		
		return msg;
	}
}
