package us.dodrill.celestia.mutuallyassured.rooms;

import us.dodrill.celestia.mutuallyassured.items.Picture;
import baavgai.textgame.framework.Area;
import baavgai.textgame.framework.DisplayItem;
import baavgai.textgame.framework.Exit;
import baavgai.textgame.framework.Player;

public class RoughShelter extends Area {
	boolean firstLook = true;
	
	public RoughShelter(Area prev) {
		this.items.add(new DisplayItem("bed", "It's a bed, its' blanket has a rainbow emblem on it that matches the one on your flank.\n"));
		this.items.add(new DisplayItem("stove", "If it worked, you could cook with it.\n"));
		this.items.add(new Picture());
		this.items.add(new Exit(new String[] {"outside", "out"}, prev));
	}
	
	protected String getLook(Player player) {
		String msg; 
		
		if(firstLook) {
			msg = "You head into the shelter. It feels surprisingly homely for not having anything on the walls.  However, there is an odd picture that you feel compelled to look at.  There is a bed too.";
			firstLook = false;
		} else {
			msg = "This shelter feels homely, a good place to take a nap.  The rainbow thunderbolt emblem on the bed matches the one on your flank.\n";
		}
		
		return msg;
	}
}
