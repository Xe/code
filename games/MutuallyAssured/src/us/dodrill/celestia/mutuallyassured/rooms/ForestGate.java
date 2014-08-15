package us.dodrill.celestia.mutuallyassured.rooms;

import us.dodrill.celestia.mutuallyassured.items.SackedItem;
import baavgai.textgame.framework.Area;
import baavgai.textgame.framework.AreaImpl;
import baavgai.textgame.framework.DisplayItem;
import baavgai.textgame.framework.Exit;
import baavgai.textgame.framework.Player;

public class ForestGate extends Area {
	boolean firstLook = true;
	Area path = new AreaImpl("As you continue north, you start to hear a stream run.  This area must have not been hit very hard by whatever catastrophe decimated the town.  Way down the path, you can see a hut.");
	
	public ForestGate(Area prev) {
		this.items.add(new Exit(Exit.DIR_S, prev));
		this.items.add(new Exit(Exit.DIR_N, path));
		path.getItems().add(new Exit(Exit.DIR_S, this));
		
		//TODO: change this
		path.getItems().add(new Exit(Exit.DIR_N, new AreaImpl("Herp derp, you're trapped")));
		
		this.items.add(new DisplayItem("trees", "These are trees, they are tall and stuff."));
		this.items.add(new SackedItem("twig", "It’s a twig, it came off of a tree at one point."));
	}
	
	protected String getLook(Player player) {
		String msg; 
		
		if(firstLook) {
			msg = "You head north more and find out that you have stumbled into a forest.  The clouds here seem to move on their own, which you mark as strange.\n";
			firstLook = false;
		} else {
			msg = "The entrance to a forest, it feels slightly spooky.";
		}
		
		return msg;
	}
}
