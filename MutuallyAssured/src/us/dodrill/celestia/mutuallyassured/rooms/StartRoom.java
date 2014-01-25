package us.dodrill.celestia.mutuallyassured.rooms;

import us.dodrill.celestia.mutuallyassured.items.Sack;
import baavgai.textgame.framework.Area;
import baavgai.textgame.framework.Exit;
import baavgai.textgame.framework.Player;

public class StartRoom extends Area {
	boolean firstLook = true;
	
	public StartRoom() {
		this.items.add(new Sack());
		this.items.add(new Exit(Exit.DIR_N, new CityStreet(this)));
	}
	
	protected String getLook(Player player) {
		String msg; 
		
		if(firstLook) {
			msg = "Suddenly, you can breathe. \n\nAll buildings around you have been mostly destroyed, as if there had been some catastrophe.  Just beneath the rubble, you can see the first signs of natural life poking through.  The ground beneath you is covered in a mixture of brick dust and ash. You ask yourself \"What the hell happened?\".  You faintly remember something important being announced, but you can't remember anything else.\nYou should grab the sack.\n";
			firstLook = false;
		} else {
			msg = "Where you woke up, there is brick dust and ash all around.\n";
		}
		
		return msg;
	}
	
	
}
