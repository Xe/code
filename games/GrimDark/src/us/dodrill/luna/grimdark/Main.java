package us.dodrill.luna.grimdark;

import us.dodrill.luna.grimdark.rooms.StartRoom;
import baavgai.textgame.framework.*;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Player player = new Player();
		player.moveTo(new StartRoom());
		//GameTerminal game = new GameTerminal(player);
		GameWindow game = new GameWindow(player);
		game.setTitle("GrimDark");
		//System.out.println("Welcome to GrimDark, the text-based adventure game.  This game takes place " + 
		//				   "with you controlling the actions of Rainbow Dash, about to meet up with Pinkie Pie " + 
		//				   "to bake cupcakes (with a twist).  I do not own these characters or the scenario.\n");
		
		game.play();
	}

}
