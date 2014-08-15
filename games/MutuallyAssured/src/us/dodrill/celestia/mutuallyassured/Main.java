package us.dodrill.celestia.mutuallyassured;

import us.dodrill.celestia.mutuallyassured.rooms.StartRoom;
import baavgai.textgame.framework.Player;
import baavgai.textgame.framework.GameWindow;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Player player = new Player();
		
		player.addMessage("Welcome to MutuallyAssured.\n\n");
		
		player.moveTo(new StartRoom());
		//GameTerminal game = new GameTerminal(player);
		GameWindow game = new GameWindow(player);
		
		game.setTitle("MutuallyAssured");
		
		game.play();
	}

}
