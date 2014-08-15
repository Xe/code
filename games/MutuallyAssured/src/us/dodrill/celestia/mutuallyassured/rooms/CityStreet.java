package us.dodrill.celestia.mutuallyassured.rooms;

import baavgai.textgame.framework.Area;
import baavgai.textgame.framework.Exit;
import baavgai.textgame.framework.Player;

public class CityStreet extends Area {
	private boolean firstLook = true;
	
	public CityStreet(Area prev) {
		this.items.add(new Exit(Exit.DIR_S, prev));
		this.items.add(new Exit(new String[] {"shelter", "to shelter"}, new RoughShelter(this)));
		this.items.add(new Exit(Exit.DIR_N, new ForestGate(this)));
	}

	protected String getLook(Player player) {
		String msg; 
		
		if(firstLook) {
			msg = "You head north at half gallop, hoping to see something other than the desolate wasteland you seem to be in.  No such luck, but here you decide to stop and rest for a minute.  The world around you doesn’t look as hopeful as where you started, but nature takes its sweet time.  You see what could be used as a shelter.\n";
			firstLook = false;
		} else {
			msg = "The roughly made shelter is here, but nothing besides that.";
		}
		
		return msg;
	}
}
